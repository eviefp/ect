{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Exporter
    ( exportFiles
    ) where

import Calendar (Calendar, Entry (..), entries, mkCalendar)
import Data.ByteString.Lazy qualified as BS
import Data.Default (def)
import Data.Hashable qualified as H
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Time.Calendar qualified as T
import Data.Time.Clock qualified as T
import Data.Time.LocalTime qualified as T
import Data.Version qualified as Version
import Text.ICalendar.Printer qualified as C
import Text.ICalendar.Types qualified as C

exportFiles :: [FilePath] -> FilePath -> IO ()
exportFiles files ics = do
    icsCalendar <- toCalendarFormat <$> mkCalendar files <*> T.getCurrentTime

    BS.writeFile ics $ C.printICalendar def icsCalendar

toCalendarFormat :: Calendar -> T.UTCTime -> C.VCalendar
toCalendarFormat events now = C.VCalendar {..}
  where
    vcProdId :: C.ProdId
    vcProdId =
        C.ProdId
            { C.prodIdValue = "ect"
            , C.prodIdOther = def
            }

    vcVersion :: C.ICalVersion
    vcVersion =
        C.MaxICalVersion
            { C.versionMax = Version.makeVersion [2, 0]
            , C.versionOther = def
            }

    vcScale :: C.Scale
    vcScale =
        C.Scale
            { C.scaleValue = "GREGORIAN"
            , C.scaleOther = def
            }

    vcMethod :: Maybe C.Method
    vcMethod =
        Just
            C.Method
                { C.methodValue = "PUBLISH"
                , C.methodOther = def
                }

    vcOther :: Set C.OtherProperty
    vcOther =
        Set.fromList
            [ C.OtherProperty
                { C.otherName = "X-WR-CALNAME"
                , C.otherValue = "Evie"
                , C.otherParams = def
                }
            , C.OtherProperty
                { C.otherName = "X-WR-TIMEZONE"
                , C.otherValue = "Europe/Bucharest"
                , C.otherParams = def
                }
            ]

    vcTimeZones :: Map Text C.VTimeZone
    vcTimeZones =
        Map.insert
            "Europe/Bucharest"
            C.VTimeZone
                { C.vtzId =
                    C.TZID
                        { C.tzidValue = "Europe/Bucharest"
                        , C.tzidGlobal = True
                        , C.tzidOther = def
                        }
                , C.vtzLastMod = Nothing
                , C.vtzUrl = Nothing
                , C.vtzStandardC =
                    Set.fromList
                        [ C.TZProp
                            { C.tzpDTStart =
                                C.DTStartDateTime
                                    { C.dtStartDateTimeValue =
                                        C.UTCDateTime
                                            T.UTCTime
                                                { T.utctDay = T.fromGregorian 1970 10 25
                                                , T.utctDayTime = T.secondsToDiffTime (60 * 60 * 4)
                                                }
                                    , C.dtStartOther = def
                                    }
                            , C.tzpTZOffsetTo =
                                C.UTCOffset
                                    { C.utcOffsetValue = 60 * 60 * 2 -- 2 hours, in seconds
                                    , C.utcOffsetOther = def
                                    }
                            , C.tzpTZOffsetFrom =
                                C.UTCOffset
                                    { C.utcOffsetValue = 60 * 60 * 3 -- 3 hours, in seconds
                                    , C.utcOffsetOther = def
                                    }
                            , C.tzpRRule =
                                Set.fromList
                                    [ C.RRule
                                        { C.rRuleValue =
                                            C.Recur
                                                { C.recurFreq = C.Yearly
                                                , C.recurUntilCount = Nothing
                                                , C.recurInterval = 1
                                                , C.recurBySecond = []
                                                , C.recurByMinute = []
                                                , C.recurByHour = []
                                                , C.recurByDay = [Left (-1, C.Sunday)]
                                                , C.recurByMonthDay = []
                                                , C.recurByYearDay = []
                                                , C.recurByWeekNo = []
                                                , C.recurByMonth = [10]
                                                , C.recurBySetPos = []
                                                , C.recurWkSt = C.Monday
                                                }
                                        , C.rRuleOther = def
                                        }
                                    ]
                            , C.tzpComment = Set.empty
                            , C.tzpRDate = Set.empty
                            , C.tzpTZName =
                                Set.fromList
                                    [ C.TZName
                                        { C.tzNameValue = "EET"
                                        , C.tzNameLanguage = Nothing
                                        , C.tzNameOther = def
                                        }
                                    ]
                            , C.tzpOther = Set.empty
                            }
                        ]
                , C.vtzDaylightC =
                    Set.fromList
                        [ C.TZProp
                            { C.tzpDTStart =
                                C.DTStartDateTime
                                    { C.dtStartDateTimeValue =
                                        C.UTCDateTime
                                            T.UTCTime
                                                { T.utctDay = T.fromGregorian 1970 03 29
                                                , T.utctDayTime = T.secondsToDiffTime (60 * 60 * 3)
                                                }
                                    , C.dtStartOther = def
                                    }
                            , C.tzpTZOffsetTo =
                                C.UTCOffset
                                    { C.utcOffsetValue = 60 * 60 * 3 -- 3 hours, in seconds
                                    , C.utcOffsetOther = def
                                    }
                            , C.tzpTZOffsetFrom =
                                C.UTCOffset
                                    { C.utcOffsetValue = 60 * 60 * 2 -- 2 hours, in seconds
                                    , C.utcOffsetOther = def
                                    }
                            , C.tzpRRule =
                                Set.fromList
                                    [ C.RRule
                                        { C.rRuleValue =
                                            C.Recur
                                                { C.recurFreq = C.Yearly
                                                , C.recurUntilCount = Nothing
                                                , C.recurInterval = 1
                                                , C.recurBySecond = []
                                                , C.recurByMinute = []
                                                , C.recurByHour = []
                                                , C.recurByDay = [Left (-1, C.Sunday)]
                                                , C.recurByMonthDay = []
                                                , C.recurByYearDay = []
                                                , C.recurByWeekNo = []
                                                , C.recurByMonth = [3]
                                                , C.recurBySetPos = []
                                                , C.recurWkSt = C.Monday
                                                }
                                        , C.rRuleOther = def
                                        }
                                    ]
                            , C.tzpComment = Set.empty
                            , C.tzpRDate = Set.empty
                            , C.tzpTZName =
                                Set.fromList
                                    [ C.TZName
                                        { C.tzNameValue = "EEST"
                                        , C.tzNameLanguage = Nothing
                                        , C.tzNameOther = def
                                        }
                                    ]
                            , C.tzpOther = Set.empty
                            }
                        ]
                , C.vtzOther =
                    Set.fromList
                        [ C.OtherProperty
                            { C.otherName = "X-LIC-LOCATION"
                            , C.otherValue = "Europe/Bucharest"
                            , C.otherParams = def
                            }
                        ]
                }
            Map.empty

    vcEvents :: Map (Text, Maybe (Either C.Date C.DateTime)) C.VEvent
    vcEvents =
        Map.fromList
            . fmap (entryToEvent now)
            . Set.toList
            $ entries events

    vcTodos :: Map (Text, Maybe (Either C.Date C.DateTime)) C.VTodo
    vcTodos = mempty

    vcJournals :: Map (Text, Maybe (Either C.Date C.DateTime)) C.VJournal
    vcJournals = mempty

    vcFreeBusys :: Map Text C.VFreeBusy
    vcFreeBusys = mempty

    vcOtherComps :: Set C.VOther
    vcOtherComps = mempty

entryToEvent :: T.UTCTime -> Entry -> ((Text, Maybe (Either C.Date C.DateTime)), C.VEvent)
entryToEvent now Entry {..} =
    ( (uid, Nothing)
    , C.VEvent
        { C.veDTStamp =
            C.DTStamp
                { C.dtStampValue = now
                , C.dtStampOther = def
                }
        , C.veUID =
            C.UID
                { C.uidValue = uid -- hash?
                , C.uidOther = def
                }
        , C.veClass =
            C.Class
                { C.classValue = C.Public
                , C.classOther = def
                }
        , C.veDTStart =
            Just
                C.DTStartDateTime
                    { C.dtStartDateTimeValue =
                        C.ZonedDateTime
                            { C.dateTimeFloating = entryStartTime
                            , C.dateTimeZone = "Europe/Bucharest"
                            }
                    , C.dtStartOther = def
                    }
        , C.veCreated =
            Just
                C.Created
                    { C.createdValue = now
                    , C.createdOther = def
                    }
        , C.veDescription = Nothing
        , C.veGeo = Nothing
        , C.veLastMod =
            Just
                C.LastModified
                    { C.lastModifiedValue = now
                    , C.lastModifiedOther = def
                    }
        , C.veLocation = Nothing
        , C.veOrganizer = Nothing
        , C.vePriority =
            C.Priority
                { C.priorityValue = 0
                , C.priorityOther = def
                }
        , C.veSeq =
            C.Sequence
                { C.sequenceValue = 0
                , C.sequenceOther = def
                }
        , C.veStatus =
            Just
                C.ConfirmedEvent
                    { C.eventStatusOther = def
                    }
        , C.veSummary =
            Just
                C.Summary
                    { C.summaryValue = T.fromStrict entryTitle
                    , C.summaryAltRep = Nothing
                    , C.summaryLanguage = Nothing
                    , C.summaryOther = def
                    }
        , C.veTransp =
            C.Opaque
                { C.timeTransparencyOther = def
                }
        , C.veUrl = Nothing
        , C.veRecurId = Nothing
        , C.veRRule = Set.empty
        , C.veDTEndDuration =
            Just $
                Left
                    C.DTEndDateTime
                        { C.dtEndDateTimeValue =
                            C.ZonedDateTime
                                { C.dateTimeFloating =
                                    fromMaybe
                                        ( T.addLocalTime
                                            (T.secondsToNominalDiffTime 3600)
                                            entryStartTime
                                        )
                                        entryEndTime
                                , C.dateTimeZone = "Europe/Bucharest"
                                }
                        , C.dtEndOther = def
                        }
        , C.veAttach = Set.empty
        , C.veAttendee = Set.empty
        , C.veCategories = Set.empty
        , C.veComment = Set.empty
        , C.veContact = Set.empty
        , C.veExDate = Set.empty
        , C.veRStatus = Set.empty
        , C.veRelated = Set.empty
        , C.veResources = Set.empty
        , C.veRDate = Set.empty
        , C.veAlarms = Set.empty
        , C.veOther = Set.empty
        }
    )
  where
    startTimeText = T.pack $ show entryStartTime
    hash = abs $ H.hash $ startTimeText <> T.fromStrict entryTitle
    uid = T.pack (show hash) <> "@calendar.eevie.ro"
