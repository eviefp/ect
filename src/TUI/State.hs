{-# LANGUAGE TemplateHaskell #-}

module TUI.State where

import Calendar qualified
import Config qualified
import TUI.Component.List qualified as List
import TUI.Component.Week qualified as Week

import Control.Lens.Combinators (makeLenses)

data Component = List | Week
  deriving (Eq)

makeLenses ''Component

data State = State
  { _calendar :: !Calendar.Calendar
  , _config :: !Config.EctConfig
  , _list :: !List.State
  , _week :: !Week.State
  , _activeComponent :: !Component
  }

makeLenses ''State
