{-# LANGUAGE TemplateHaskell #-}

module TUI
  ( run
  ) where

import Calendar qualified
import Config qualified
import TUI.Component.List qualified as List
import TUI.Component.Week qualified as Week

import Brick qualified

import Data.Text (Text)
import Data.Time qualified as Time

import Control.Lens (makeLenses, (.=), (^.))
import Control.Monad.State qualified as State

import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Input qualified as VtyInput

data Component = List | Week
  deriving (Eq)

makeLenses ''Component

data State = State
  { _calendar :: !Calendar.Calendar
  , _list :: !List.State
  , _week :: !Week.State
  , _activeComponent :: !Component
  }

makeLenses ''State

draw :: State -> [Brick.Widget Text]
draw state =
  case state ^. activeComponent of
    List -> List.draw (state ^. list)
    Week -> Week.draw (state ^. week)

handleEvent
  :: Brick.BrickEvent Text ()
  -> Brick.EventM Text State ()
handleEvent =
  \case
    Brick.AppEvent _ -> pure ()
    Brick.MouseDown {} -> pure ()
    Brick.MouseUp {} -> pure ()
    event@(Brick.VtyEvent vtyEvent) -> do
      case vtyEvent of
        VtyInput.EvKey VtyInput.KEsc _ -> Brick.halt
        VtyInput.EvKey (VtyInput.KChar 'q') _ -> Brick.halt
        VtyInput.EvKey (VtyInput.KChar 'w') _ ->
          activeComponent .= Week
        VtyInput.EvKey (VtyInput.KChar 'a') _ ->
          activeComponent .= List
        _otherwise -> do
          state <- State.get
          case state ^. activeComponent of
            List -> do
              newList <- Brick.nestEventM' (state ^. list) $ List.handleEvent event
              list .= newList
            Week -> do
              newList <-
                Brick.nestEventM' (state ^. week) $
                  Week.handleEvent (state ^. calendar) event
              week .= newList

build :: Calendar.Calendar -> Time.LocalTime -> State
build _calendar now =
  let
    _list = List.build _calendar now
    _week = Week.build True _calendar (Time.localDay now)
    _activeComponent = Week
  in
    State {..}

mkAttrMap :: Brick.AttrMap
mkAttrMap =
  Brick.attrMap
    (Brick.fg Attr.white)
    [ (Brick.attrName "highlight", Brick.fg Attr.green)
    ]

app :: Brick.App State () Text
app =
  Brick.App
    { Brick.appDraw = draw
    , Brick.appChooseCursor = \_ _ -> Nothing
    , Brick.appHandleEvent = handleEvent
    , Brick.appStartEvent = pure ()
    , Brick.appAttrMap = const mkAttrMap
    }

run :: Config.EctConfig -> IO ()
run config = do
  now <- Calendar.now
  _calendar <- Calendar.fromConfig config
  _ <- Brick.defaultMain app $ build _calendar now
  pure ()
