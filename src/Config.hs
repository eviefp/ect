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
getConfig = Dir.getXdgDirectory Dir.XdgConfig defaultConfigPath >>= Yaml.decodeFileThrow

-- getConfig = Yaml.decodeFileThrow testConfigPath

data EctConfig = EctConfig
    { calendars :: ![EctCalendarConfig]
    , notification :: !EctNotificationConfig
    , export :: !EctExportConfig
    }
    deriving stock (Generic)
instance Aeson.FromJSON EctConfig

data EctCalendarConfig = EctCalendarConfig
    { name :: !Text
    , importFrom :: !(Maybe Text)
    , path :: !Text
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
    , output :: !Text
    }
    deriving stock (Generic)
instance Aeson.FromJSON EctExportConfig
