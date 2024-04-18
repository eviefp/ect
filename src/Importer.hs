{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Importer
    ( importFiles
    ) where

import Config qualified

import Data.Default (def)
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable (fold, traverse_)
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Text qualified as Strict
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.IO qualified as T
import Data.Time.Clock qualified as Clock
import Data.Time.Format qualified as Time
import Data.Time.LocalTime qualified as Time
import Network.Wreq qualified as Wreq
import Text.ICalendar.Parser qualified as CalParser
import Text.ICalendar.Types qualified as C

testConfig :: [Config.EctCalendarConfig]
testConfig =
    [ Config.EctCalendarConfig
        { Config.name = "evie"
        , Config.inputUrl =
            "https://calendar.google.com/calendar/ical/alexaeviest%40gmail.com/private-18b1e7d8a6f6e36d9785321c45b5f249/basic.ics"
        , Config.outputPath = "./test/evie.org"
        , Config.shouldExport = False
        }
    , Config.EctCalendarConfig
        { Config.name = "gia-evie"
        , Config.inputUrl =
            "https://calendar.google.com/calendar/ical/s810p67l2bi1168j8luka5nic0%40group.calendar.google.com/private-65e582415f3a020468b41cd5a94e0dee/basic.ics"
        , Config.outputPath = "./test/gia-evie.org"
        , Config.shouldExport = False
        }
    , Config.EctCalendarConfig
        { Config.name = "proton"
        , Config.inputUrl =
            "https://calendar.proton.me/api/calendar/v1/url/vS55jZbaHpS1tE80KarCJpkJJ_ukgKGcsw_HpLnRXX1vgkLAEumSnR1JNJkcPCu3aZ0iEnysujzcbaFw1g3c4w==/calendar.ics?CacheKey=QQLJfxqoMKM2YM85uIaF7g%3D%3D&PassphraseKey=Og_-Aimhwq7Z6BRCMLPnttwCQypQPQy2U2GjYzvT74U%3D"
        , Config.outputPath = "./test/proton.org"
        , Config.shouldExport = False
        }
    ]

importFiles :: IO ()
importFiles = traverse_ importFile testConfig

importFile :: Config.EctCalendarConfig -> IO (Either String ())
importFile Config.EctCalendarConfig {..} = do
    input <- fold <$> Wreq.get (Strict.unpack inputUrl)
    let
        result = CalParser.parseICalendar def (Strict.unpack name) input
    case result of
        Left err -> do
            putStrLn "error"
            putStrLn err
            pure $ Left err
        Right ([vc], []) -> do
            writeToOrgFile name outputPath vc
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
            putStrLn $ "running for " <> Strict.unpack name
            let
                text = foldMap (eventToOrgText name) (Map.elems $ C.vcEvents cal)
            T.writeFile (Strict.unpack outputPath) $ Builder.toLazyText text
            pure $ Right ()

eventToOrgText :: Strict.Text -> C.VEvent -> Builder
eventToOrgText name C.VEvent {..} =
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
            , created
            , lastMod
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

    created :: Builder
    created = maybe mempty (mkProperty "Created" (fromDate . C.createdValue)) veCreated

    lastMod :: Builder
    lastMod = maybe mempty (mkProperty "LastModified" (fromDate . C.lastModifiedValue)) veLastMod

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
        veDTStart >>= \case
            C.DTStartDateTime {..} ->
                case dtStartDateTimeValue of
                    C.FloatingDateTime {..} -> Just dateTimeFloating
                    C.UTCDateTime {..} -> Just $ Time.utcToLocalTime Time.utc dateTimeUTC
                    C.ZonedDateTime {..} -> Just dateTimeFloating
            C.DTStartDate {..} ->
                Just $ Time.LocalTime (C.dateValue dtStartDateValue) Time.midnight

    startDate :: Builder
    startDate = maybe mempty (mappend "  " . fromDate) startLocalTime

    endDate :: Builder
    endDate =
        case startLocalTime of
            Nothing -> mempty
            Just startTime ->
                case veDTEndDuration of
                    Nothing -> mempty
                    Just (Left C.DTEndDateTime {..}) ->
                        "--" <> fromDateTime dtEndDateTimeValue
                    Just (Left C.DTEndDate {..}) ->
                        "--" <> fromDate (C.dateValue dtEndDateValue)
                    Just (Right C.DurationProp {..}) ->
                        "--" <> fromDate (Time.addLocalTime (diffTime durationValue) startTime)

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

fromDateTime :: C.DateTime -> Builder
fromDateTime =
    \case
        C.FloatingDateTime d -> fromDate d
        C.UTCDateTime d -> fromDate d
        C.ZonedDateTime d _ -> fromDate d

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

fromDate :: forall t. (Time.FormatTime t) => t -> Builder
fromDate time =
    "<"
        <> Builder.fromString
            (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %a %H:%M" time)
        <> ">"
