{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Calendar
    ( Entry (..)
    , Calendar
    , mkCalendar
    , entries
    , now
    , display
    ) where

import Chronos qualified as C
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as IO
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Org.Parser qualified as OrgParser
import Org.Types qualified as Org

now :: (MonadIO m) => m Org.TimestampData
now =
    (C.timeToDatetime <$> IO.liftIO C.now)
        >>= \case
            C.Datetime {..} ->
                let
                    C.Date {..} = datetimeDate
                    C.TimeOfDay {..} = datetimeTime
                    date =
                        ( C.getYear dateYear
                        , C.getMonth dateMonth + 1
                        , C.getDayOfMonth dateDay
                        , Just . dayOfWeek $ C.dateToDayOfWeek datetimeDate
                        )
                    time = (timeOfDayHour + 2, timeOfDayMinute)
                in
                    pure $ Org.TimestampData False (date, Just time, Nothing, Nothing)

dayOfWeek :: C.DayOfWeek -> Text
dayOfWeek = C.caseDayOfWeek (C.buildDayOfWeekMatch "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")

display :: Entry -> Text
display Entry {..} = time entryDates <> " " <> entryTitle
  where
    time :: Org.TimestampData -> Text
    time tsd = case getStartDate tsd of
        (_, Just (h, m), _, _) -> ishow h <> ":" <> ishow m
        -- All day evetnts:
        _ -> "*"

    ishow :: Int -> Text
    ishow i
        | i < 10 = "0" <> T.pack (show i)
        | otherwise = T.pack (show i)

data Entry = Entry
    { entryTitle :: !Text
    , entryDates :: !Org.TimestampData
    , entrySection :: !(Maybe Org.OrgSection)
    }
    deriving stock (Eq)

instance Ord Entry where
    compare :: Entry -> Entry -> Ordering
    compare = compare `on` (getStartDate . entryDates)

getStartDate :: Org.TimestampData -> Org.DateTime
getStartDate = \case
    Org.TimestampData _ st -> st
    Org.TimestampRange _ st _ -> st

newtype Calendar = Calendar (Set Entry)

entries :: Calendar -> Set Entry
entries (Calendar s) = s

mkCalendar :: (MonadIO m) => [FilePath] -> m Calendar
mkCalendar files =
    Calendar
        . Set.fromList
        . concatMap (mapMaybe sectionToEntry . Org.documentSections)
        <$> traverse (OrgParser.parseOrgDocIO OrgParser.defaultOrgOptions) files

sectionToEntry :: Org.OrgSection -> Maybe Entry
sectionToEntry section@Org.OrgSection {..} =
    case findEntryDates sectionChildren of
        Nothing -> Nothing
        Just entryDates ->
            Just $ Entry {..}
  where
    entryTitle = sectionRawTitle

    entrySection = Just section

    findEntryDates :: [Org.OrgElement] -> Maybe Org.TimestampData
    findEntryDates = \case
        [] -> Nothing
        (x : xs) ->
            case Org.elementData x of
                Org.Paragraph paragraphs -> findDatesInParagraphs paragraphs
                _ -> findEntryDates xs

    findDatesInParagraphs :: [Org.OrgObject] -> Maybe Org.TimestampData
    findDatesInParagraphs = \case
        [] -> Nothing
        (x : xs) ->
            case x of
                Org.Timestamp tdata -> Just tdata
                _ -> findDatesInParagraphs xs
