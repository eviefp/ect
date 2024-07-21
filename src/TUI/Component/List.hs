module TUI.Component.List
  ( State,
    draw,
    handleEvent,
    build,
  )
where

import Brick qualified
import Brick.Widgets.List qualified as Brick.List
import Calendar qualified
import Control.Monad.State qualified as State
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Time qualified as Time
import TUI.Events qualified

newtype State = State
  { getState :: Brick.List.GenericList Text Seq Calendar.Entry
  }

draw :: State -> [Brick.Widget Text]
draw state =
  [ Brick.hLimit 100
      . Brick.vLimitPercent 100
      $ Brick.List.renderList (TUI.Events.renderEvent True TUI.Events.fullTimeFormat) True
      $ getState state
  ]

handleEvent :: Brick.BrickEvent Text () -> Brick.EventM Text State ()
handleEvent =
  \case
    Brick.VtyEvent event -> do
      state <- State.get
      newList <-
        Brick.nestEventM' (getState state) $
          Brick.List.handleListEventVi (const (pure ())) event
      State.put $ State newList
    _otherwise -> pure ()

build :: Calendar.Calendar -> Time.LocalTime -> State
build calendar now =
  State
    . TUI.Events.mkEvents "ect-all-events"
    . Seq.fromList
    . Calendar.entriesAfter 100 now False
    $ calendar
