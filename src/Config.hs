{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

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
