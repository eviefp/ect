{-# LANGUAGE DeriveAnyClass #-}

module Ect (main) where

import Calendar qualified as C
import Config qualified
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
import Data.Foldable (traverse_)
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
import Debug.Trace qualified as Debug
import Exporter qualified
import Foreign qualified
import GHC.Generics (Generic)
import HttpServer qualified
import Importer qualified
import Network.Socket qualified as Network
import Options.Generic qualified as Opt
import System.Process qualified as Process
import TUI qualified

socketAddress :: String
socketAddress = "/tmp/ect/ect.socket.sock"

data Options
  = Next {config :: Maybe FilePath, value :: Int}
  | Inc {config :: Maybe FilePath}
  | Dec {config :: Maybe FilePath}
  | Server {config :: Maybe FilePath}
  | Upcoming {config :: Maybe FilePath, value :: Int}
  | Export {config :: Maybe FilePath}
  | Import {config :: Maybe FilePath}
  | Tui {config :: Maybe FilePath}
  deriving stock (Generic, Show)
  deriving anyclass (Opt.ParseRecord)

data Command = Increment | Decrement | NextCommand !Int

runModeToText :: Options -> Maybe Text
runModeToText = \case
  Next _ k -> Just $ T.pack $ show k <> "\n"
  Inc _ -> Just "inc\n"
  Dec _ -> Just "dec\n"
  _otherwise -> Nothing

textToRunMode :: Text -> Command
textToRunMode input =
  case TextRead.decimal input of
    Left _ ->
      if input == "inc\n"
        then Increment
        else
          if input == "dec\n"
            then Decrement
            else NextCommand 0
    Right (n, _) -> do
      NextCommand n

main :: IO ()
main = do
  options <- Opt.getRecord "ect"
  cfg <- Config.getConfig options.config
  cal <- C.fromConfig cfg
  case options of
    Server _ -> do
      calTvar <- STM.newTVarIO cal
      Async.mapConcurrently_
        id
        [ updateCalendar cfg calTvar
        , runDaemon calTvar
        , runNotifications cfg calTvar
        , HttpServer.run cfg.export
        ]
    Upcoming _ k -> processUpcoming cal True k
    Export _ -> runExport cfg
    Import _ -> do
      _ <- Importer.importFiles cfg.calendars
      pure ()
    Tui _ -> TUI.run cfg
    rm -> do
      socket <- Network.socket Network.AF_UNIX Network.Stream Network.defaultProtocol
      Network.connect socket $ Network.SockAddrUnix socketAddress
      case runModeToText rm of
        Nothing -> Network.close socket *> error "Internal error: unmatched run mode."
        Just command -> do
          TF.useAsPtr command $ \ptr len ->
            void $ Network.sendBuf socket ptr (fromEnum len)
          Network.close socket

processUpcoming :: C.Calendar -> Bool -> Int -> IO ()
processUpcoming cal showRecent n = do
  now <- C.now
  let
    result = fmap (C.fromEntry now) . C.entriesAfter n now showRecent $ cal
  Debug.traceShowM result
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
    threadsRef <- Ref.newIORef Map.empty

    forever do
      now <- C.now
      cal <- STM.readTVarIO tcal

      let
        newEntries = C.entriesAfter numThreads now False cal
      threads <- Ref.readIORef threadsRef
      newThreads <- mkNewThreadsMap now threads newEntries

      -- cancel threads that don't exist anymore
      traverse_ Async.cancel
        . Map.elems
        $ Map.difference threads newThreads

      -- update threads list
      Ref.writeIORef threadsRef newThreads

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
    Foreign.allocaBytes 1024 \buffer -> do
      size <- Network.recvBuf conn buffer 1024
      result <- TF.fromPtr buffer (toEnum size)
      case textToRunMode result of
        NextCommand n -> STM.atomically $ STM.writeTVar tskip n
        Increment -> STM.atomically $ STM.modifyTVar tskip (+ 1)
        Decrement -> STM.atomically $ STM.modifyTVar tskip (\k -> max 0 (k - 1))

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
      pure $ safeLast $ C.entriesAfter (skip + 1) now False cal
    maybe mempty (sendEntry conn) entry'

    forever do
      entry <- STM.atomically $ STM.readTChan peerChannel
      sendEntry conn entry

  broadcastTimer :: STM.TChan C.Entry -> STM.TVar Int -> IO ()
  broadcastTimer broadcast tskip = do
    initialNow <- C.now
    initialCal <- STM.readTVarIO tcal
    lastResultRef <- Ref.newIORef $ safeLast $ C.entriesAfter 1 initialNow False initialCal
    lastSkipRef <- Ref.newIORef 0

    forever do
      Conc.threadDelay 1_000_000
      now <- C.now
      cal <- STM.readTVarIO tcal
      lastResult <- Ref.readIORef lastResultRef
      skip <- STM.readTVarIO tskip
      let
        result = safeLast $ C.entriesAfter (skip + 1) now False cal
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
