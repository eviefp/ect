{-# LANGUAGE TemplateHaskell #-}

module TUI.Component.Week
  ( State
  , draw
  , handleEvent
  , build
  ) where

import Calendar qualified

import Brick qualified
import Brick.Widgets.List qualified as Brick.List

import Control.Lens.Combinators (makeLenses)
import Control.Monad.State qualified as State

import Brick ((<+>), (<=>))
import Brick.Widgets.Border qualified as Border
import Brick.Widgets.Center qualified as BrickCenter
import Control.Lens (ALens', cloneLens, (.=), (^.))
import Control.Monad (when)
import Control.Monad.IO.Class qualified as IO
import Data.Foldable (traverse_)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Time qualified as Time
import Graphics.Vty.Input qualified as VtyInput
import TUI.Events qualified as TuiEvents

data Day = Day
  { _items :: !(Brick.List.GenericList Text Seq Calendar.Entry)
  , _active :: !Bool
  }
makeLenses ''Day

data State = State
  { _monday :: !Day
  , _tuesday :: !Day
  , _wednesday :: !Day
  , _thursday :: !Day
  , _friday :: !Day
  , _saturday :: !Day
  , _sunday :: !Day
  , _day :: !Time.Day
  }

makeLenses ''State

draw :: State -> [Brick.Widget Text]
draw state =
  let
    start = state ^. day
    end = Time.addDays 6 start
  in
    [ BrickCenter.hCenter
        ( TuiEvents.formatTime TuiEvents.dayFormat start
            <+> Brick.txt " - "
            <+> TuiEvents.formatTime TuiEvents.dayFormat end
        )
        <=> Brick.txt ""
        <=> ( drawWeekDay start state._monday
                <+> Brick.txt " "
                <+> drawWeekDay (Time.addDays 1 start) state._tuesday
                <+> Brick.txt " "
                <+> drawWeekDay (Time.addDays 2 start) state._wednesday
                <+> Brick.txt " "
                <+> drawWeekDay (Time.addDays 3 start) state._thursday
                <+> Brick.txt " "
                <+> drawWeekDay (Time.addDays 4 start) state._friday
                <+> Brick.txt " "
                <+> drawWeekDay (Time.addDays 5 start) state._saturday
                <+> Brick.txt " "
                <+> drawWeekDay (Time.addDays 6 start) state._sunday
            )
    ]
 where
  drawWeekDay :: Time.Day -> Day -> Brick.Widget Text
  drawWeekDay date currentDay =
    Border.hBorderWithLabel
      ( TuiEvents.withAttrIf currentDay._active "highlight" $
          TuiEvents.formatTime " %a, %d %b " date
      )
      <=> Brick.vLimitPercent
        100
        ( Brick.List.renderList
            (TuiEvents.renderEvent currentDay._active TuiEvents.hourFormat)
            currentDay._active
            currentDay._items
        )

handleEvent :: Calendar.Calendar -> Brick.BrickEvent Text () -> Brick.EventM Text State ()
handleEvent calendar =
  \case
    Brick.VtyEvent keyEvent -> do
      state <- State.get
      case keyEvent of
        VtyInput.EvKey (VtyInput.KChar 'n') _ -> do
          now <- Time.localDay <$> IO.liftIO Calendar.now
          State.put $ build True calendar now
        VtyInput.EvKey (VtyInput.KChar 'h') _ -> do
          if state._monday._active
            then do
              let
                day' = Time.addDays (-7) state._day
              State.put $ build False calendar day'
            else do
              (`traverse_` zip (reverse weekdays) (drop 1 $ reverse weekdays)) \(this, prev) -> do
                when (state ^. cloneLens this . active) do
                  cloneLens this . active .= False
                  cloneLens prev . active .= True
        VtyInput.EvKey (VtyInput.KChar 'l') _ -> do
          if state._sunday._active
            then do
              let
                day' = Time.addDays 7 state._day
              State.put $ build True calendar day'
            else do
              (`traverse_` zip weekdays (drop 1 weekdays)) \(this, next) ->
                when (state ^. cloneLens this . active) do
                  cloneLens this . active .= False
                  cloneLens next . active .= True
        _o -> do
          (`traverse_` weekdays) \weekday ->
            when (state ^. cloneLens weekday . active) do
              newList <-
                Brick.nestEventM' (state ^. cloneLens weekday . items) $
                  Brick.List.handleListEventVi (const (pure ())) keyEvent
              cloneLens weekday . items .= newList
    _otherwise -> pure ()

build :: Bool -> Calendar.Calendar -> Time.Day -> State
build start calendar now =
  let
    (mon, tue, wed, thu, fri, sat, sun) =
      case Seq.fromList . Calendar.entriesFor calendar <$> Time.weekAllDays Time.Monday now of
        [_1, _2, _3, _4, _5, _6, _7] -> (_1, _2, _3, _4, _5, _6, _7)
        _o -> (Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty, Seq.empty)

    _day = head $ Time.weekAllDays Time.Monday now

    _monday = Day (TuiEvents.mkEvents "ect-week-monday" mon) start
    _tuesday = Day (TuiEvents.mkEvents "ect-week-tuesday" tue) False
    _wednesday = Day (TuiEvents.mkEvents "ect-week-wednesday" wed) False
    _thursday = Day (TuiEvents.mkEvents "ect-week-thursday" thu) False
    _friday = Day (TuiEvents.mkEvents "ect-week-friday" fri) False
    _saturday = Day (TuiEvents.mkEvents "ect-week-saturday" sat) False
    _sunday = Day (TuiEvents.mkEvents "ect-week-sunday" sun) (not start)
  in
    State {..}

weekdays :: [ALens' State Day]
weekdays = [monday, tuesday, wednesday, thursday, friday, saturday, sunday]
