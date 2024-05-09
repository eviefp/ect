module Ect (main) where

import Calendar qualified as C
import Config qualified
import Exporter qualified
import Foreign qualified
import HttpServer qualified
import Importer qualified
import TUI qualified

import Control.Concurrent qualified as Conc
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.Thread.Delay qualified as Delays
import Control.Exception qualified as E
import Control.Monad (forever, when)

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BS
import Data.Fixed qualified as Time
import Data.Functor (void)
import Data.IORef qualified as Ref
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Foreign qualified as TF
import Data.Text.IO qualified as T
import Data.Text.Read qualified as TextRead
import Data.Time qualified as Time

import Network.Socket qualified as Network
import System.Environment qualified as Env
import System.Process qualified as Process
import Text.Read qualified as R

socketAddress :: String
socketAddress = "/tmp/ect/ect.socket.sock"

data RunMode
    = Next !Int
    | Inc
    | Dec
    | Server
    | Upcoming !Int
    | Export
    | Import
    | Tui

parseArgs :: [String] -> Maybe RunMode
parseArgs = \case
    ["--skip"] -> Just $ Next 0
    ["--skip", k] -> Just $ Next (fromMaybe 0 $ R.readMaybe k)
    ["--up"] -> Just Inc
    ["--down"] -> Just Dec
    ["--server"] -> Just Server
    ["--upcoming"] -> Just $ Upcoming 10
    ["--upcoming", k] -> Just $ Upcoming (fromMaybe 10 $ R.readMaybe k)
    ["--export"] -> Just Export
    ["--import"] -> Just Import
    ["tui"] -> Just Tui
    _k -> Just Tui

runModeToText :: RunMode -> Maybe Text
runModeToText = \case
    Server -> Nothing
    Next k -> Just $ T.pack $ show k <> "\n"
    Inc -> Just "inc\n"
    Dec -> Just "dec\n"
    Upcoming _ -> Nothing
    Export -> Nothing
    Import -> Nothing
    Tui -> Nothing

textToRunMode :: Text -> RunMode
textToRunMode input =
    case TextRead.decimal input of
        Left _ ->
            if input == "inc\n"
                then Inc
                else
                    if input == "dec\n"
                        then Dec
                        else Next 0
        Right (n, _) -> do
            Next n

eval :: RunMode -> IO ()
eval runMode = do
    config <- Config.getConfig
    cal <- C.fromConfig config
    case runMode of
        Server -> do
            calTvar <- STM.newTVarIO cal
            Async.mapConcurrently_
                id
                [ updateCalendar config calTvar
                , runDaemon calTvar
                , runNotifications config calTvar
                , HttpServer.run config.export
                ]
        Upcoming k -> processUpcoming cal k
        Export -> runExport config
        Import -> do
            _ <- Importer.importFiles config.calendars
            pure ()
        Tui -> TUI.run config
        rm -> do
            socket <- Network.socket Network.AF_UNIX Network.Stream Network.defaultProtocol
            Network.connect socket $ Network.SockAddrUnix socketAddress
            case runModeToText rm of
                Nothing -> Network.close socket *> error "Internal error: unmatched run mode."
                Just command -> do
                    TF.useAsPtr command $ \ptr len ->
                        void $ Network.sendBuf socket ptr (fromEnum len)
                    Network.close socket

processUpcoming :: C.Calendar -> Int -> IO ()
processUpcoming cal n = do
    now <- C.now
    let
        result = fmap C.UpcomingEntry . C.entriesAfter n now $ cal
    T.putStrLn . T.decodeUtf8 . BS.toStrict . Aeson.encode $ result

updateCalendar :: Config.EctConfig -> TVar C.Calendar -> IO ()
updateCalendar config tcal = forever do
    -- import and export calendars first
    when config.export.enable do
        Importer.importFiles config.calendars
        runExport config

    -- update in-memory cache
    C.fromConfig config >>= STM.atomically . STM.writeTVar tcal

    -- pause for 1 minute
    Conc.threadDelay 60_000_000

runNotifications :: Config.EctConfig -> TVar C.Calendar -> IO ()
runNotifications config tcal = do
    when config.notification.enable do
        threads <- Ref.newIORef Map.empty
        forever do
            now <- C.now
            cal <- STM.readTVarIO tcal
            let
                newEntries = C.entriesAfter numThreads now cal
            newThreads <- Ref.readIORef threads >>= flip (mkNewThreadsMap now) newEntries
            Ref.writeIORef threads newThreads

            Conc.threadDelay 60_000_000
  where
    numThreads :: Int
    numThreads = Config.threads . Config.notification $ config

    notificationSetting :: Text
    notificationSetting = Config.exec . Config.notification $ config

    needle :: Text
    needle = "{title}"

    mkNewThreadsMap
        :: Time.LocalTime -> Map C.Entry (Async ()) -> [C.Entry] -> IO (Map C.Entry (Async ()))
    mkNewThreadsMap _ _ [] = pure Map.empty
    mkNewThreadsMap now threads (x : xs) = do
        t <- case Map.lookup x threads of
            Just t -> pure t
            Nothing -> mkThread (toMicroseconds now $ C._entryStartTime x) x
        Map.insert x t <$> mkNewThreadsMap now threads xs

    toMicroseconds :: Time.LocalTime -> Time.LocalTime -> Integer
    toMicroseconds now tsd =
        case Time.nominalDiffTimeToSeconds $ Time.diffLocalTime tsd now of
            Time.MkFixed i ->
                let
                    result = i `div` 1_000_000
                in
                    max 1_000_000 (result - 15 * 60_000_000)

    mkThread :: Integer -> C.Entry -> IO (Async ())
    mkThread delay C.Entry {..} =
        Async.async do
            Delays.delay delay
            run _entryTitle

    run :: Text -> IO ()
    run title =
        Process.callCommand . T.unpack . T.replace needle title $ notificationSetting

runDaemon :: TVar C.Calendar -> IO ()
runDaemon tcal = Network.withSocketsDo do
    broadcast <- STM.newBroadcastTChanIO
    tskip <- STM.newTVarIO (0 :: Int)
    Async.concurrently_
        (E.bracket open Network.close (loop broadcast tskip))
        (broadcastTimer broadcast tskip)
  where
    open :: IO Network.Socket
    open = do
        socket <- Network.socket Network.AF_UNIX Network.Stream Network.defaultProtocol
        Network.bind socket $ Network.SockAddrUnix socketAddress
        Network.listen socket 1024
        pure socket

    loop :: STM.TChan C.Entry -> STM.TVar Int -> Network.Socket -> IO ()
    loop chan tskip socket = forever do
        E.bracketOnError (Network.accept socket) (Network.close . fst) $
            \(conn, _peer) -> void do
                peerChannel <- STM.atomically $ STM.dupTChan chan
                Async.asyncWithUnmask \_ ->
                    Async.concurrently_
                        (receiveSkipUpdates tskip conn)
                        (sendUpdates peerChannel tskip conn)

    receiveSkipUpdates :: STM.TVar Int -> Network.Socket -> IO ()
    receiveSkipUpdates tskip conn = forever do
        buffer <- Foreign.mallocBytes 1024
        size <- Network.recvBuf conn buffer 1024
        result <- TF.fromPtr buffer (toEnum size)
        case textToRunMode result of
            Next n -> STM.atomically $ STM.writeTVar tskip n
            Inc -> STM.atomically $ STM.modifyTVar tskip (+ 1)
            Dec -> STM.atomically $ STM.modifyTVar tskip (\k -> max 0 (k - 1))
            Server -> pure ()
            _otherwise -> pure ()

    sendEntry :: Network.Socket -> C.Entry -> IO ()
    sendEntry conn entry = do
        let
            result = T.decodeUtf8 . BS.toStrict . Aeson.encode $ entry
        TF.useAsPtr (result <> "\n") $ \ptr len ->
            void $ Network.sendBuf conn ptr (fromEnum len)

    sendUpdates :: STM.TChan C.Entry -> TVar Int -> Network.Socket -> IO ()
    sendUpdates peerChannel tskip conn = do
        now <- C.now
        entry' <- STM.atomically do
            skip <- STM.readTVar tskip
            cal <- STM.readTVar tcal
            pure $ safeLast $ C.entriesAfter (skip + 1) now cal
        maybe mempty (sendEntry conn) entry'

        forever do
            entry <- STM.atomically $ STM.readTChan peerChannel
            sendEntry conn entry

    broadcastTimer :: STM.TChan C.Entry -> STM.TVar Int -> IO ()
    broadcastTimer broadcast tskip = do
        initialNow <- C.now
        initialCal <- STM.readTVarIO tcal
        lastResultRef <- Ref.newIORef $ safeLast $ C.entriesAfter 1 initialNow initialCal
        lastSkipRef <- Ref.newIORef 0

        forever do
            Conc.threadDelay 1_000_000
            now <- C.now
            cal <- STM.readTVarIO tcal
            lastResult <- Ref.readIORef lastResultRef
            skip <- STM.readTVarIO tskip
            let
                result = safeLast $ C.entriesAfter (skip + 1) now cal
            when (lastResult /= result) do
                Ref.writeIORef lastResultRef result
                lastSkip <- Ref.readIORef lastSkipRef
                STM.atomically do
                    when (lastSkip == skip) $ STM.writeTVar tskip (max 0 (skip - 1))
                    STM.writeTChan broadcast $ fromMaybe C.emptyEntry result
                Ref.writeIORef lastSkipRef skip

safeLast :: [C.Entry] -> Maybe C.Entry
safeLast =
    \case
        [] -> Nothing
        [x] -> Just x
        (_ : xs) -> safeLast xs

runExport :: Config.EctConfig -> IO ()
runExport Config.EctConfig {..} =
    let
        shouldExportCalendars = (T.unpack . Config.path <$> filter Config.shouldExport calendars)
    in
        Exporter.exportFiles shouldExportCalendars (T.unpack $ Config.outputPath export)

main :: IO ()
main = do
    Env.getArgs >>= maybe showError eval . parseArgs

showError :: IO ()
showError = error "Could not parse input arguments."
