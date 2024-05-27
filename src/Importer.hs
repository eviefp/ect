module Importer
    ( importFiles
    ) where

import Calendar qualified
import Config qualified

import Data.Default (def)
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable (fold, traverse_)
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Strict
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.IO qualified as T
import Data.Time qualified as Time
import Data.Time.Calendar.Month qualified as Time
import Data.Time.Clock qualified as Clock

import Network.Wreq qualified as Wreq

import Text.ICalendar.Parser qualified as CalParser
import Text.ICalendar.Types qualified as C

importFiles :: [Config.EctCalendarConfig] -> IO ()
importFiles = traverse_ importFile

importFile :: Config.EctCalendarConfig -> IO (Either String ())
importFile Config.EctCalendarConfig {..} =
    case importFrom of
        Nothing -> pure $ Right ()
        Just url -> do
            input <- fold <$> Wreq.get (Strict.unpack url)
            let
                result = CalParser.parseICalendar def (Strict.unpack name) input
            case result of
                Left err -> do
                    putStrLn "error"
                    putStrLn err
                    pure $ Left err
                Right ([vc], []) -> do
                    writeToOrgFile name path vc
                Right (vc, str) -> do
                    putStrLn $ "vc: " <> show (length vc) <> ", errors: " <> show (length str)
                    pure $ Left $ "vc: " <> show (length vc) <> ", errors: " <> show (length str)

writeToOrgFile :: Strict.Text -> Strict.Text -> C.VCalendar -> IO (Either String ())
writeToOrgFile name outputPath cal = do
    if
        | Map.size (C.vcTodos cal) > 0 ->
            pure $ Left "todos > 0"
        | Map.size (C.vcJournals cal) > 0 ->
            pure $ Left "journals > 0"
        | Map.size (C.vcFreeBusys cal) > 0 ->
            pure $ Left "freeBusys > 0"
        | otherwise -> do
            now <- Calendar.now
            timezone <- Time.getCurrentTimeZone
            let
                timezoneInfo = parseTimezones now <$> C.vcTimeZones cal
                text = foldMap (eventToOrgText name timezoneInfo timezone) (Map.elems $ C.vcEvents cal)

            T.writeFile (Strict.unpack outputPath) $ Builder.toLazyText text
            pure $ Right ()

parseTimezones :: Time.LocalTime -> C.VTimeZone -> Time.TimeZone
parseTimezones now tz = Time.minutesToTimeZone offset
  where
    offset :: Int
    offset = maybe 0 (C.utcOffsetValue . C.tzpTZOffsetTo) current `div` 60

    standard :: [C.TZProp]
    standard = Set.toList . C.vtzStandardC $ tz

    daylight :: [C.TZProp]
    daylight = Set.toList . C.vtzDaylightC $ tz

    current :: Maybe C.TZProp
    current =
        case (standard, daylight) of
            ([std], []) -> Just std
            ([std], [dls]) -> match std dls
            _o -> Nothing

    getRecur :: C.TZProp -> Maybe C.Recur
    getRecur = fmap C.rRuleValue . listToMaybe . Set.toList . C.tzpRRule

    year :: Integer
    year = case Time.toGregorian (Time.localDay now) of (y, _, _) -> y

    byWeekDay :: Int -> Time.DayOfWeek -> [Time.Day]
    byWeekDay m weekDay =
        let
            period :: Time.Month = Time.dayPeriod (Time.fromGregorian year m 1)
        in
            [day | day <- Time.periodAllDays period, Time.dayOfWeek day == weekDay]

    mkLocalTime :: C.Recur -> Time.LocalTime
    mkLocalTime C.Recur {..} = Time.LocalTime day' Time.midnight
      where
        recurMonth =
            case recurByMonth of
                [m] -> m
                _o -> 1
        (recurWeekDay, recurCount) =
            case recurByDay of
                [Left (n, weekDay)] -> (toDayOfWeek weekDay, n)
                _o -> (Time.Sunday, 1)

        day' =
            if recurCount < 0
                then head . drop (abs recurCount - 1) . reverse . byWeekDay recurMonth $ recurWeekDay
                else head . drop (abs recurCount - 1) . byWeekDay recurMonth $ recurWeekDay

    toDayOfWeek :: C.Weekday -> Time.DayOfWeek
    toDayOfWeek =
        \case
            C.Monday -> Time.Monday
            C.Tuesday -> Time.Tuesday
            C.Wednesday -> Time.Wednesday
            C.Thursday -> Time.Thursday
            C.Friday -> Time.Friday
            C.Saturday -> Time.Saturday
            C.Sunday -> Time.Sunday

    match :: C.TZProp -> C.TZProp -> Maybe C.TZProp
    match std dl =
        case (getRecur std, getRecur dl) of
            (Nothing, Nothing) -> Nothing
            (Just stdRecur, Just dlRecur) ->
                let
                    standardDay = mkLocalTime stdRecur
                    daylightDay = mkLocalTime dlRecur
                in
                    if standardDay > daylightDay
                        then
                            if now > standardDay && now < daylightDay
                                then Just std
                                else Just dl
                        else
                            if now > daylightDay && now < standardDay
                                then Just dl
                                else Just std
            (Just _, _) -> Just std
            (_, Just _) -> Just dl

eventToOrgText :: Strict.Text -> Map Text Time.TimeZone -> Time.TimeZone -> C.VEvent -> Builder
eventToOrgText name timezones localTimeZone C.VEvent {..} =
    fold
        [ title
        , tag
        , newline
        , startDate
        , endDate
        , newline
        , drawer
        , description
        , newline
        , newline
        ]
  where
    title :: Builder
    title =
        case veSummary of
            Nothing -> "* untitled"
            Just C.Summary {..} -> "* " <> Builder.fromLazyText summaryValue

    tag :: Builder
    tag = " :" <> Builder.fromText name <> ":"

    description :: Builder
    description =
        case veDescription of
            Nothing -> mempty
            Just C.Description {..} ->
                Builder.fromLazyText descriptionValue

    newline :: Builder
    newline = "\n"

    drawer :: Builder
    drawer =
        fold
            [ "  :PROPERTIES:"
            , newline
            , uid
            , class_
            , location
            , organizer
            , sequence__
            , status
            , transparency
            , "  :END:"
            , newline
            ]

    mkDrawerProperty :: Text -> Builder -> Builder
    mkDrawerProperty prop value =
        fold
            [ "  :"
            , Builder.fromLazyText prop
            , ": "
            , Builder.fromLazyText (Text.replicate (30 - Text.length prop) " ")
            , value
            , newline
            ]

    mkProperty :: forall a. Text -> (a -> Builder) -> a -> Builder
    mkProperty prop fn value =
        mkDrawerProperty prop (fn value)

    mkTextProperty :: forall a. Text -> (a -> Text) -> a -> Builder
    mkTextProperty prop fn value =
        mkDrawerProperty prop (Builder.fromLazyText $ fn value)

    mkStringProperty :: forall a. Text -> (a -> String) -> a -> Builder
    mkStringProperty prop fn value =
        mkDrawerProperty prop (Builder.fromString $ fn value)

    uid :: Builder
    uid = mkTextProperty "UID" C.uidValue veUID

    class_ :: Builder
    class_ = mkStringProperty "Class" (show . C.classValue) veClass

    location :: Builder
    location = maybe mempty (mkTextProperty "Location" C.locationValue) veLocation

    organizer :: Builder
    organizer = maybe mempty (mkProperty "Organizer" (const "TODO")) (Just ())

    sequence__ :: Builder
    sequence__ = mkStringProperty "Sequence" (show . C.sequenceValue) veSeq

    status :: Builder
    status = maybe mempty (mkProperty "Status" statusToBuilder) veStatus

    transparency :: Builder
    transparency = mkProperty "Transparency" transparencyToBuilder veTransp

    startLocalTime :: Maybe Time.LocalTime
    startLocalTime =
        veDTStart <&> \case
            C.DTStartDateTime {..} -> fromDateTime timezones localTimeZone dtStartDateTimeValue
            C.DTStartDate {..} -> Time.LocalTime (C.dateValue dtStartDateValue) Time.midnight

    startDate :: Builder
    startDate = maybe mempty ((\r -> "  <" <> r <> recurrence <> ">") . formatDate) startLocalTime

    recurrence :: Builder
    recurrence = -- mkDrawerProperty "Recurrence" $ Builder.fromString $ show veRRule
      case Set.toList veRRule of
        [ C.RRule (C.Recur {..}) _ ] ->
          if recurFreq == C.Weekly
            then case length recurByDay of
                    1 -> " +" <> Builder.fromString (show recurInterval) <> "w"
                    5 -> " +" <> Builder.fromString (show recurInterval) <> "x"
                    7 -> " +" <> Builder.fromString (show recurInterval) <> "d"
                    _ -> ""
            else ""
        _otherwise -> ""

    endDate :: Builder
    endDate =
        case startLocalTime of
            Nothing -> mempty
            Just startTime ->
                case veDTEndDuration of
                    Nothing -> mempty
                    Just (Left C.DTEndDateTime {..}) ->
                        "--<" <> formatDate (fromDateTime timezones localTimeZone dtEndDateTimeValue) <> ">"
                    Just (Left C.DTEndDate {..}) ->
                        "--" <> formatDate (C.dateValue dtEndDateValue) <> ">"
                    Just (Right C.DurationProp {..}) ->
                        "--" <> formatDate (Time.addLocalTime (diffTime durationValue) startTime) <> ">"

    diffTime :: C.Duration -> Clock.NominalDiffTime
    diffTime d =
        Clock.secondsToNominalDiffTime
            case d of
                C.DurationDate {..} ->
                    signToNumber durSign
                        * Foldable.sum
                            [ MkFixed (toInteger durDay) * day
                            , MkFixed (toInteger durHour) * hour
                            , MkFixed (toInteger durMinute) * minute
                            , MkFixed (toInteger durSecond)
                            ]
                C.DurationTime {..} ->
                    signToNumber durSign
                        * Foldable.sum
                            [ MkFixed (toInteger durHour) * hour
                            , MkFixed (toInteger durMinute) * minute
                            , MkFixed (toInteger durSecond)
                            ]
                C.DurationWeek {..} ->
                    signToNumber durSign * MkFixed (toInteger durWeek) * week
      where
        minute = 60
        hour = 60 * minute
        day = 24 * hour
        week = day * 7
        signToNumber =
            \case
                C.Positive -> 1
                C.Negative -> (-1)

fromDateTime :: Map Text Time.TimeZone -> Time.TimeZone -> C.DateTime -> Time.LocalTime
fromDateTime timezones local =
    \case
        C.FloatingDateTime {..} -> dateTimeFloating
        C.UTCDateTime {..} -> Time.utcToLocalTime local dateTimeUTC
        C.ZonedDateTime {..} ->
            let
                tz = fromMaybe local $ Map.lookup dateTimeZone timezones
            in
                Time.utcToLocalTime local $ Time.localTimeToUTC tz dateTimeFloating

transparencyToBuilder :: C.TimeTransparency -> Builder
transparencyToBuilder =
    \case
        C.Opaque _ -> "Opaque"
        C.Transparent _ -> "Transparent"

statusToBuilder :: C.EventStatus -> Builder
statusToBuilder =
    \case
        C.TentativeEvent _ -> "Tentative"
        C.ConfirmedEvent _ -> "Confirmed"
        C.CancelledEvent _ -> "Cancelled"

formatDate :: forall t. (Time.FormatTime t) => t -> Builder
formatDate time =
        Builder.fromString
            (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %a %H:%M" time)
