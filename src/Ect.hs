{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Ect (main) where

import Calendar qualified as C

import Data.Aeson qualified as Aeson
import Data.Foldable (traverse_)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as T
import Data.Text.Lazy.IO qualified as T
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import System.Directory qualified as Dir

data EctConfig = EctConfig
    { calendars :: ![Text]
    , notification :: !Text
    }
    deriving stock (Generic)

instance Aeson.FromJSON EctConfig

defaultConfigPath :: FilePath
defaultConfigPath = "ect/ect.yaml"

main :: IO ()
main = do
    config <- Dir.getXdgDirectory Dir.XdgConfig defaultConfigPath >>= Yaml.decodeFileThrow

    cal <- C.mkCalendar (T.unpack <$> calendars config)

    now <- C.now

    traverse_ T.putStr
        . fmap (T.decodeUtf8 . Aeson.encode)
        . S.lookupGE (C.Entry mempty now Nothing)
        . C.entries
        $ cal
