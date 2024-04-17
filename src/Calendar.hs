{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Calendar
    ( Entry (..)
    , UpcomingEntry (..)
    , Calendar
    , mkCalendar
    , entries
    , now
    , entriesAfter
    , getNth
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as IO
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Function (on)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time qualified as Time
import Data.Time.Calendar.MonthDay qualified as Time
import Data.Time.Calendar.OrdinalDate qualified as Time
import GHC.Generics (Generic)
import Org.Parser qualified as OrgParser
import Org.Types qualified as Org

now :: (MonadIO m) => m Time.LocalTime
now = Time.zonedTimeToLocalTime <$> IO.liftIO Time.getZonedTime

getNth :: Int -> Time.LocalTime -> Calendar -> Maybe Entry
getNth n now' =
    (Set.lookupMin . Set.drop n . snd . Set.split (Entry mempty now' Nothing Nothing)) . entries

data Entry = Entry
    { entryTitle :: !Text
    , entryStartTime :: !Time.LocalTime
    , entryEndTime :: !(Maybe Time.LocalTime)
    , entrySection :: !(Maybe Org.OrgSection)
    }
    deriving stock (Eq, Generic)

instance Ord Entry where
    compare :: Entry -> Entry -> Ordering
    compare = compare `on` entryStartTime

instance Aeson.ToJSON Entry where
    toJSON :: Entry -> Aeson.Value
    toJSON Entry {..} =
        Aeson.object
            [ "title" .= entryTitle
            , "time" .= printTime entryStartTime
            , "date" .= printDate entryStartTime
            , "duration"
                .= round @_ @Int ((/ 60) (Time.diffLocalTime (fromMaybe entryStartTime entryEndTime) entryStartTime))
            ]

    toEncoding :: Entry -> Aeson.Encoding
    toEncoding Entry {..} =
        Aeson.pairs $
            "title" .= entryTitle
                <> "time" .= printTime entryStartTime
                <> "date" .= printDate entryStartTime
                <> "duration"
                    .= round @_ @Int ((/ 60) (Time.diffLocalTime (fromMaybe entryStartTime entryEndTime) entryStartTime))

printTime :: Time.LocalTime -> String
printTime = Time.formatTime Time.defaultTimeLocale "%H:%M"

printDate :: Time.LocalTime -> String
printDate = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"

newtype UpcomingEntry = UpcomingEntry {getUpcomingEntry :: Entry}
    deriving newtype (Eq)

instance Aeson.ToJSON UpcomingEntry where
    toJSON :: UpcomingEntry -> Aeson.Value
    toJSON UpcomingEntry {getUpcomingEntry = Entry {..}} =
        Aeson.object
            [ "title" .= entryTitle
            , "time" .= printTime entryStartTime
            , "date" .= printUpcomingDate entryStartTime
            , "duration"
                .= round @_ @Int ((/ 60) (Time.diffLocalTime (fromMaybe entryStartTime entryEndTime) entryStartTime))
            ]

    toEncoding :: UpcomingEntry -> Aeson.Encoding
    toEncoding UpcomingEntry {getUpcomingEntry = Entry {..}} =
        Aeson.pairs $
            "title" .= entryTitle
                <> "time" .= printTime entryStartTime
                <> "date" .= printUpcomingDate entryStartTime
                <> "duration"
                    .= round @_ @Int ((/ 60) (Time.diffLocalTime (fromMaybe entryStartTime entryEndTime) entryStartTime))

printUpcomingDate :: Time.LocalTime -> String
printUpcomingDate = Time.formatTime Time.defaultTimeLocale "%A, %d %b"

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
    case findEntryStartTime sectionChildren of
        Nothing -> Nothing
        Just (entryStartTime, entryEndTime) ->
            Just $ Entry {..}
  where
    entryTitle = sectionRawTitle

    entrySection = Just section

    findEntryStartTime :: [Org.OrgElement] -> Maybe (Time.LocalTime, Maybe Time.LocalTime)
    findEntryStartTime = \case
        [] -> Nothing
        (x : xs) ->
            case Org.elementData x of
                Org.Paragraph paragraphs -> findDatesInParagraphs paragraphs
                _otherwise -> findEntryStartTime xs

    findDatesInParagraphs :: [Org.OrgObject] -> Maybe (Time.LocalTime, Maybe Time.LocalTime)
    findDatesInParagraphs = \case
        [] -> Nothing
        (x : xs) ->
            case x of
                Org.Timestamp (Org.TimestampData _ start) -> (,Nothing) <$> parseTimestampData start
                Org.Timestamp (Org.TimestampRange _ start end) -> (,parseTimestampData end) <$> parseTimestampData start
                _otherwise -> findDatesInParagraphs xs

    parseTimestampData :: Org.DateTime -> Maybe Time.LocalTime
    parseTimestampData ((y, m, d, _), mtime, _, _) = do
        let
            year = toInteger y
            isLeapYear = Time.isLeapYear year
        dayOfYear <- Time.monthAndDayToDayOfYearValid isLeapYear m d
        localDay <- Time.fromOrdinalDateValid year dayOfYear
        localTimeOfDay <-
            case mtime of
                Nothing -> Just Time.midnight
                Just (hour, minute) -> Time.makeTimeOfDayValid hour minute 0
        pure $ Time.LocalTime {..}

entriesAfter :: Int -> Entry -> Set Entry -> Set Entry
entriesAfter k entry = Set.take k . snd . Set.split entry
