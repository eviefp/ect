{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ect (main) where

import Calendar qualified as C

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
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Foreign qualified as TF
import Data.Text.IO qualified as T
import Data.Text.Read qualified as TextRead
import Data.Time qualified as Time
import Data.Yaml qualified as Yaml
import Exporter qualified
import Foreign qualified
import GHC.Generics (Generic)
import Network.Socket qualified as Network
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.Process qualified as Process
import Text.Read qualified as R

socketAddress :: String
socketAddress = "/tmp/ect/ect.socket.sock"

data EctNotificationConfig = EctNotificationConfig
    { exec :: !Text
    , threads :: !Int
    }
    deriving stock (Generic)

instance Aeson.FromJSON EctNotificationConfig

data EctExportConfig = EctExportConfig
    { enable :: !Bool
    , calendars :: ![Text]
    , output :: !Text
    }
    deriving stock (Generic)

instance Aeson.FromJSON EctExportConfig

data EctConfig = EctConfig
    { calendars :: ![Text]
    , notification :: !EctNotificationConfig
    , export :: !EctExportConfig
    }
    deriving stock (Generic)

instance Aeson.FromJSON EctConfig

defaultConfigPath :: FilePath
defaultConfigPath = "ect/ect.yaml"

data RunMode
    = Next !Int
    | Inc
    | Dec
    | Server
    | Upcoming !Int
    | Export

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
    _k -> Nothing

runModeToText :: RunMode -> Maybe Text
runModeToText = \case
    Server -> Nothing
    Next k -> Just $ T.pack $ show k <> "\n"
    Inc -> Just "inc\n"
    Dec -> Just "dec\n"
    Upcoming _ -> Nothing
    Export -> Nothing

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
    config <- Dir.getXdgDirectory Dir.XdgConfig defaultConfigPath >>= Yaml.decodeFileThrow
    cal <- C.mkCalendar (T.unpack <$> config.calendars)
    case runMode of
        Server -> do
            calTvar <- STM.newTVarIO cal
            Async.mapConcurrently_
                id
                [ updateCalendar config calTvar
                , runDaemon calTvar
                , runNotifications config calTvar
                , runExporter (export config)
                ]
        Upcoming k -> processUpcoming cal k
        Export -> runExporter (export config)
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
        result =
            -- List.groupWith (Time.localDay . C.entryStartTime)
            fmap C.UpcomingEntry
                . S.toAscList
                . C.entriesAfter n (C.Entry mempty now Nothing)
                . C.entries
                $ cal
    T.putStrLn . T.decodeUtf8 . BS.toStrict . Aeson.encode $ result

updateCalendar :: EctConfig -> TVar C.Calendar -> IO ()
updateCalendar config tcal = forever do
    cal <- C.mkCalendar (T.unpack <$> config.calendars)
    STM.atomically . STM.writeTVar tcal $ cal
    Conc.threadDelay 60_000_000 -- one minute

runNotifications :: EctConfig -> TVar C.Calendar -> IO ()
runNotifications config tcal = do
    threads <- Ref.newIORef Map.empty
    forever do
        now <- C.now
        cal <- STM.readTVarIO tcal
        let
            newEntries = S.toList . C.entriesAfter numThreads (C.Entry mempty now Nothing) . C.entries $ cal
        newThreads <- Ref.readIORef threads >>= flip (mkNewThreadsMap now) newEntries
        Ref.writeIORef threads newThreads

        Conc.threadDelay 60_000_000
  where
    numThreads :: Int
    numThreads = threads . notification $ config

    notificationSetting :: Text
    notificationSetting = exec . notification $ config

    needle :: Text
    needle = "{title}"

    mkNewThreadsMap
        :: Time.LocalTime -> Map C.Entry (Async ()) -> [C.Entry] -> IO (Map C.Entry (Async ()))
    mkNewThreadsMap _ _ [] = pure Map.empty
    mkNewThreadsMap now threads (x : xs) = do
        t <- case Map.lookup x threads of
            Just t -> pure t
            Nothing -> mkThread (toMicroseconds now $ C.entryStartTime x) x
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
            run entryTitle

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
        Conc.threadDelay 5_000_000

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
            pure $ C.getNth skip now cal
        maybe mempty (sendEntry conn) entry'

        forever do
            entry <- STM.atomically $ STM.readTChan peerChannel
            sendEntry conn entry

    broadcastTimer :: STM.TChan C.Entry -> STM.TVar Int -> IO ()
    broadcastTimer broadcast tskip = do
        initialNow <- C.now
        initialCal <- STM.readTVarIO tcal
        lastResultRef <- Ref.newIORef $ C.getNth 0 initialNow initialCal
        lastSkipRef <- Ref.newIORef 0

        forever do
            Conc.threadDelay 1_000_000
            now <- C.now
            cal <- STM.readTVarIO tcal
            lastResult <- Ref.readIORef lastResultRef
            skip <- STM.readTVarIO tskip
            let
                result = C.getNth skip now cal
            when (lastResult /= result) do
                Ref.writeIORef lastResultRef result
                lastSkip <- Ref.readIORef lastSkipRef
                STM.atomically do
                    when (lastSkip == skip) $ STM.writeTVar tskip (max 0 (skip - 1))
                    STM.writeTChan broadcast (fromMaybe (C.Entry "Calendar is empty." now Nothing) result)
                Ref.writeIORef lastSkipRef skip

runExporter :: EctExportConfig -> IO ()
runExporter EctExportConfig {..} =
    when enable $ forever do
        Exporter.exportFiles (T.unpack <$> calendars) (T.unpack output)
        Conc.threadDelay $ 1_000_000 * 60 * 10 -- 10 minutes

main :: IO ()
main = do
    Env.getArgs >>= maybe showError eval . parseArgs

showError :: IO ()
showError = error "Could not parse input arguments."
