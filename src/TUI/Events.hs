module TUI.Events
  ( mkEvents
  , renderEvent
  , withAttrIf
  , formatTime
  , fullTimeFormat
  , dayFormat
  , hourFormat
  ) where

import Brick ((<+>), (<=>))
import Brick qualified
import Brick.Widgets.Border qualified as Border
import Brick.Widgets.List qualified as BL
import Calendar qualified
import Data.Foldable (foldl')
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time qualified as Time

mkEvents :: Text -> Seq Calendar.Entry -> BL.GenericList Text Seq Calendar.Entry
mkEvents name entries = BL.list name entries 4

fullTimeFormat :: String
fullTimeFormat = "%A, %d %B %H:%M"

dayFormat :: String
dayFormat = "%A, %d %B"

hourFormat :: String
hourFormat = "%H:%M"

renderEvent :: Bool -> String -> Bool -> Calendar.Entry -> Brick.Widget Text
renderEvent isActive format hasFocus Calendar.Entry {..} =
  withAttrIf (isActive && hasFocus) "highlight" $
    Brick.txt _entryTitle
      <=> formatTime format _entryStartTime
      <=> ( Brick.txt "["
              <+> foldl' (<+>) Brick.emptyWidget (Brick.padRight (Brick.Pad 2) . Brick.txt <$> _entryTags)
              <+> Brick.txt "]"
          )
      <=> Border.hBorder

formatTime :: (Time.FormatTime t) => String -> t -> Brick.Widget n
formatTime tf = Brick.str . Time.formatTime Time.defaultTimeLocale tf

withAttrIf :: Bool -> String -> Brick.Widget n -> Brick.Widget n
withAttrIf condition attrName widget =
  if condition
    then Brick.withAttr (Brick.attrName attrName) widget
    else widget
