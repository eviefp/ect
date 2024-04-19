{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import System.Directory qualified as Dir

defaultConfigPath :: FilePath
defaultConfigPath = "ect/ect.yaml"

testConfigPath :: FilePath
testConfigPath = "./ect.yaml"

getConfig :: IO EctConfig
getConfig = Yaml.decodeFileThrow testConfigPath

-- getConfig = Dir.getXdgDirectory Dir.XdgConfig defaultConfigPath >>= Yaml.decodeFileThrow

data EctConfig = EctConfig
    { calendars :: ![EctCalendarConfig]
    , notification :: !EctNotificationConfig
    , export :: !EctExportConfig
    }
    deriving stock (Generic)
instance Aeson.FromJSON EctConfig

data EctCalendarConfig = EctCalendarConfig
    { name :: !Text
    , inputUrl :: !Text
    , outputPath :: !Text
    , shouldExport :: !Bool
    }
    deriving stock (Generic)
instance Aeson.FromJSON EctCalendarConfig

data EctNotificationConfig = EctNotificationConfig
    { exec :: !Text
    , threads :: !Int
    }
    deriving stock (Generic)
instance Aeson.FromJSON EctNotificationConfig

data EctExportConfig = EctExportConfig
    { enable :: !Bool
    , extraCalendars :: ![Text]
    , output :: !Text
    }
    deriving stock (Generic)
instance Aeson.FromJSON EctExportConfig
