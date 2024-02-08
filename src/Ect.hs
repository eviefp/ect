{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ect (main) where

import Calendar qualified as C

import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as T
import Data.Text.Lazy.IO qualified as T
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import System.Directory qualified as Dir
import System.Environment qualified as Env
import Text.Read qualified as R

data EctConfig = EctConfig
    { calendars :: ![Text]
    , notification :: !Text
    }
    deriving stock (Generic)

instance Aeson.FromJSON EctConfig

defaultConfigPath :: FilePath
defaultConfigPath = "ect/ect.yaml"

data RunMode
    = Next !Int

parseArgs :: [String] -> RunMode
parseArgs = \case
    ("--skip" : k : _) -> Next (fromMaybe 0 $ R.readMaybe k)
    _ -> Next 0

find :: Int -> C.Entry -> Set C.Entry -> Maybe C.Entry
find k entry = S.lookupMin . S.drop k . snd . S.split entry

eval :: RunMode -> IO ()
eval = \case
    Next n -> do
        config <- Dir.getXdgDirectory Dir.XdgConfig defaultConfigPath >>= Yaml.decodeFileThrow

        cal <- C.mkCalendar (T.unpack <$> calendars config)

        now <- C.now

        traverse_ T.putStr
            . fmap (T.decodeUtf8 . Aeson.encode)
            . find n (C.Entry mempty now Nothing)
            . C.entries
            $ cal

main :: IO ()
main = do
    Env.getArgs >>= eval . parseArgs
