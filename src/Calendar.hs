{-# LANGUAGE TemplateHaskell #-}

module Calendar
    ( Entry (..)
    , entryTitle
    , entryTags
    , entryStartTime
    , entryEndTime
    , entryDescription
    , entryProperties
    , entrySection
    , emptyEntry
    , entryRepeat
    , Repeat (..)
    , _Daily
    , _Workdays
    , _Week
    , _Monthly
    , _Yearly
    , RepeatUntil (..)
    , _RUDate
    , Properties (..)
    , propertyUid
    , propertyClass
    , propertyLocation
    , propertyOrganizer
    , propertySequence
    , propertyStatus
    , propertyTransparency
    , UpcomingEntry (..)
    , Calendar (calendarEntries)
    , mkCalendar
    , fromConfig
    , now
    , entriesAfter
    , entriesFor
    , expandRepeat
    )
where

import Config qualified
import Control.Lens (makeLenses, makePrisms, (%~), (&))
import Control.Lens.Prism (_Just)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as IO
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Function (on)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder
import Data.Time qualified as Time
import Data.Time.Calendar.MonthDay qualified as Time
import Data.Time.Calendar.OrdinalDate qualified as Time
import GHC.Generics (Generic)
import Org.Parser qualified as OrgParser
import Org.Types qualified as Org
import Org.Walk qualified as Org
import Control.Applicative ((<|>))
import Text.Read (readMaybe)

data Repeat
    = Daily
    | Workdays
    | Week
    | Monthly
    | Yearly
    deriving stock (Eq, Generic)

makePrisms ''Repeat

data RepeatUntil
    = RUDate !Time.LocalTime
    | RUCount !Int
    deriving stock (Eq, Generic)

makePrisms ''RepeatUntil

data Properties = Properties
    { _propertyUid :: !Text
    , _propertyClass :: !Text
    , _propertyLocation :: !(Maybe Text)
    , _propertyOrganizer :: !(Maybe Text)
    , _propertySequence :: !Int
    , _propertyStatus :: !(Maybe Text)
    , _propertyTransparency :: !Text
    , _propertyRepeatUntil :: !(Maybe RepeatUntil)
    }
    deriving stock (Eq, Generic)

makeLenses ''Properties

data Entry = Entry
    { _entryTitle :: !Text
    , _entryTags :: ![Text]
    , _entryStartTime :: !Time.LocalTime
    , _entryEndTime :: !(Maybe Time.LocalTime)
    , _entryRepeat :: !(Maybe Repeat)
    , _entryDescription :: !Text
    , _entryProperties :: !Properties
    , _entrySection :: !(Maybe Org.OrgSection)
    }
    deriving stock (Eq, Generic)

makeLenses ''Entry

newtype Calendar = Calendar {calendarEntries :: [Entry]}

mkCalendar :: (MonadIO m) => ([Entry] -> [Entry]) -> [FilePath] -> m Calendar
mkCalendar f files =
    Calendar
        . List.sortOn _entryStartTime
        . f
        . concatMap walkEntries
        <$> traverse (OrgParser.parseOrgDocIO OrgParser.defaultOrgOptions) files

fromConfig :: (MonadIO m) => Config.EctConfig -> m Calendar
fromConfig config = do
    currentTime <- now
    let
        calendars = T.unpack . Config.path <$> config.calendars
     in
        mkCalendar (concatMap (expandRepeat currentTime)) calendars

expandRepeat :: Time.LocalTime -> Entry -> [Entry]
expandRepeat currentTime entry@Entry {..} = go <$> addTime
  where
    pastLimit :: Time.LocalTime
    pastLimit = Time.addLocalTime (negate $ Time.nominalDay * 30) currentTime

    workdays :: [Time.DayOfWeek]
    workdays = [Time.Monday, Time.Tuesday, Time.Wednesday, Time.Thursday, Time.Friday]

    repeatUntil :: Time.LocalTime
    repeatUntil =
        case _entryProperties._propertyRepeatUntil of
            Nothing -> Time.addLocalTime (1000 * Tome.nominalDay) currentTime
            Just (RUCount _) -> Time.addLocalTime (1000 * Tome.nominalDay) currentTime -- TODO: this is probably wrong
            Just (RUDate d) -> d

    addTime :: [Time.NominalDiffTime]
    addTime =
        if repeatUntil < currentTime
            then []
            else case _entryRepeat of
                Nothing -> [0]
                Just r -> case r of
                    Daily ->
                        if _entryStartTime > currentTime
                            then [Time.nominalDay * k | k <- [0, 1 .. 60]]
                            else
                                take 60 . dropWhile ((>) pastLimit . flip Time.addLocalTime _entryStartTime) $
                                    [Time.nominalDay * k | k <- [0, 1 ..]]
                    Workdays ->
                        if _entryStartTime > currentTime
                            then
                                take 60 $
                                    filter ((`elem` workdays) . Time.dayOfWeek . Time.localDay . flip Time.addLocalTime _entryStartTime) $
                                        [Time.nominalDay * k | k <- [0, 1 ..]]
                            else
                                take 60
                                    . filter ((`elem` workdays) . Time.dayOfWeek . Time.localDay . flip Time.addLocalTime _entryStartTime)
                                    . dropWhile ((>) pastLimit . flip Time.addLocalTime _entryStartTime)
                                    $ [Time.nominalDay * k | k <- [0, 1 ..]]
                    Week ->
                        if _entryStartTime > currentTime
                            then [Time.nominalDay * k | k <- [0, 7 .. 60]]
                            else
                                take 60 . dropWhile ((>) pastLimit . flip Time.addLocalTime _entryStartTime) $
                                    [Time.nominalDay * k | k <- [0, 7 ..]]
                    Monthly -> []
                    Yearly -> []

    go :: Time.NominalDiffTime -> Entry
    go diff =
        entry
            & entryStartTime %~ Time.addLocalTime diff
            & entryEndTime . _Just %~ Time.addLocalTime diff

now :: (MonadIO m) => m Time.LocalTime
now = Time.zonedTimeToLocalTime <$> IO.liftIO Time.getZonedTime

entriesAfter :: Int -> Time.LocalTime -> Calendar -> [Entry]
entriesAfter k localTime = take k . go . calendarEntries
  where
    go :: [Entry] -> [Entry]
    go =
        \case
            [] -> []
            (x : xs)
                | _entryStartTime x > localTime -> x : xs
                | otherwise -> go xs

entriesFor :: Calendar -> Time.Day -> [Entry]
entriesFor cal day = go $ calendarEntries cal
  where
    go :: [Entry] -> [Entry]
    go =
        \case
            [] -> []
            (x : xs)
                | (Time.localDay . _entryStartTime $ x) == day -> x : go xs
                | otherwise -> go xs

emptyEntry :: Entry
emptyEntry =
    let
        _entryTitle = "Calendar is empty."
        _entryTags = []
        _entryStartTime = Time.LocalTime (toEnum 0) Time.midnight
        _entryEndTime = Nothing
        _entryDescription = ""
        _entryProperties = emptyProperties
        _entrySection = Nothing
        _entryRepeat = Nothing
    in
        Entry {..}

emptyProperties :: Properties
emptyProperties =
    let
        _propertyUid = ""
        _propertyClass = ""
        _propertyLocation = Nothing
        _propertyOrganizer = Nothing
        _propertySequence = 0
        _propertyStatus = Nothing
        _propertyTransparency = ""
        _propertyRepeatUntil = Nothing
    in
        Properties {..}

instance Ord Entry where
    compare :: Entry -> Entry -> Ordering
    compare = compare `on` _entryStartTime

instance Aeson.ToJSON Entry where
    toJSON :: Entry -> Aeson.Value
    toJSON Entry {..} =
        Aeson.object
            [ "title" .= _entryTitle
            , "time" .= printTime _entryStartTime
            , "date" .= printDate _entryStartTime
            , "duration"
                .= round @_ @Int
                    ((/ 60) (Time.diffLocalTime (fromMaybe _entryStartTime _entryEndTime) _entryStartTime))
            ]

    toEncoding :: Entry -> Aeson.Encoding
    toEncoding Entry {..} =
        Aeson.pairs $
            "title" .= _entryTitle
                <> "time" .= printTime _entryStartTime
                <> "date" .= printDate _entryStartTime
                <> "duration"
                    .= round @_ @Int
                        ((/ 60) (Time.diffLocalTime (fromMaybe _entryStartTime _entryEndTime) _entryStartTime))

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
            [ "title" .= _entryTitle
            , "time" .= printTime _entryStartTime
            , "date" .= printUpcomingDate _entryStartTime
            , "duration"
                .= round @_ @Int
                    ((/ 60) (Time.diffLocalTime (fromMaybe _entryStartTime _entryEndTime) _entryStartTime))
            ]

    toEncoding :: UpcomingEntry -> Aeson.Encoding
    toEncoding UpcomingEntry {getUpcomingEntry = Entry {..}} =
        Aeson.pairs $
            "title" .= _entryTitle
                <> "time" .= printTime _entryStartTime
                <> "date" .= printUpcomingDate _entryStartTime
                <> "duration"
                    .= round @_ @Int
                        ((/ 60) (Time.diffLocalTime (fromMaybe _entryStartTime _entryEndTime) _entryStartTime))

printUpcomingDate :: Time.LocalTime -> String
printUpcomingDate = Time.formatTime Time.defaultTimeLocale "%a, %d %b"

walkEntries :: Org.OrgDocument -> [Entry]
walkEntries = Org.query go -- TODO: does this go deep?
  where
    go :: Org.OrgSection -> [Entry]
    go section =
        case getFirst $ foldMap (Org.query findDatesInParagraphs) (Org.sectionChildren section) of
            Nothing -> []
            Just (_entryStartTime, _entryEndTime, _entryRepeat) ->
                let
                    _entryTitle = Org.sectionRawTitle section
                    _entryTags = Org.sectionTags section
                    _entryProperties = mkProperties $ Org.sectionProperties section
                    _entryDescription =
                        LazyText.toStrict $
                            Builder.toLazyText $
                                foldMap (Org.query mkDescription) (Org.sectionChildren section)
                    _entrySection = Just section
                in
                    [Entry {..}]

mkDescription :: Org.OrgElement -> Builder.Builder
mkDescription Org.OrgElement {..} =
    case elementData of
        Org.Clock _ _ -> "TODO: Clock"
        Org.GreaterBlock _ _ -> "TODO: GreaterBlock"
        Org.Drawer {..} -> "Drawer: " <> Builder.fromText drawerName
        Org.PlainList _ _ -> "TODO: PlainList"
        Org.ExportBlock _ _ -> "TODO: ExportBlock"
        Org.ExampleBlock _ _ -> "TODO: ExampleBlock"
        Org.SrcBlock {} -> "TODO: SrcBlock"
        Org.VerseBlock _ -> "TODO: VerseBlock"
        Org.HorizontalRule -> "TODO: HorizontalRule"
        Org.Keyword _ _ -> "TODO: Keyword"
        Org.LaTeXEnvironment _ _ -> "TODO: Latex"
        Org.Paragraph o -> foldMap printObject o
        Org.Table _ -> "TODO: Table"
        Org.FootnoteDef _ _ -> "TODO: Footnote"
        Org.Comment -> ""

printObject :: Org.OrgObject -> Builder.Builder
printObject =
    \case
        Org.Plain t -> Builder.fromText t
        Org.LineBreak -> "\n"
        Org.Italic xs -> "/" <> foldMap printObject xs <> "/"
        Org.Underline xs -> "_" <> foldMap printObject xs <> "_"
        Org.Bold xs -> "*" <> foldMap printObject xs <> "*"
        Org.Strikethrough xs -> "+" <> foldMap printObject xs <> "+"
        Org.Superscript xs -> "^{" <> foldMap printObject xs <> "}"
        Org.Subscript xs -> "_{" <> foldMap printObject xs <> "}"
        Org.Quoted Org.SingleQuote xs -> "'" <> foldMap printObject xs <> "'"
        Org.Quoted Org.DoubleQuote xs -> "\"" <> foldMap printObject xs <> "\""
        Org.Code t -> "~" <> Builder.fromText t <> "~"
        Org.Verbatim t -> "=" <> Builder.fromText t <> "="
        Org.Timestamp _ts -> "TODO: Timestamp"
        Org.Entity _e -> "TODO: Entity"
        Org.LaTeXFragment _ft _t -> "TODO: LaTeX"
        Org.ExportSnippet _be _val -> "TODO: Snippet"
        Org.FootnoteRef _frd -> "TODO: Footnote"
        Org.Cite _c -> "TODO: Citation"
        Org.InlBabelCall _b -> "TODO: BabelCall"
        Org.Src _lang _param _val -> "TODO: src"
        Org.Link _target _obj -> "TODO: Link"
        Org.Target _id _text -> "TODO: Target"
        Org.Macro _name _args -> "TODO: Macro"
        Org.StatisticCookie _s -> "TODO: StatisticCookie"

mkProperties :: Org.Properties -> Properties
mkProperties props =
    let
        get key = Map.lookup key props
        getDef key def = fromMaybe def $ get key

        _propertyUid = getDef "uid" ""
        _propertyClass = getDef "class" ""
        _propertyLocation = get "location"
        _propertyOrganizer = get "organizer"
        _propertySequence = read . T.unpack $ getDef "sequence" "0"
        _propertyStatus = get "status"
        _propertyTransparency = getDef "transparency" ""
        _propertyRepeatUntil = get "repeatuntil" >>= parseRepeatUntil
    in
        Properties {..}
  where
    parseRepeatUntil :: Text -> Maybe RepeatUntil
    parseRepeatUntil t = parseRepeatDate t <|> parseRepeatCount t

    parseRepeatCount :: Text -> Maybe RepeatUntil
    parseRepeatCount = fmap RUCount . readMaybe . T.unpack

    parseRepeatDate :: Text -> Maybe RepeatUntil
    parseRepeatDate = fmap RUDate . Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %a %H:%M" . T.unpack

findDatesInParagraphs :: Org.OrgObject -> First (Time.LocalTime, Maybe Time.LocalTime, Maybe Repeat)
findDatesInParagraphs = \case
    Org.Timestamp (Org.TimestampData _ start) -> First $ (\(mst, mrep) -> (mst, Nothing, mrep)) <$> parseTimestampData start
    Org.Timestamp (Org.TimestampRange _ start end) -> First $ do
        (s, mrep) <- parseTimestampData start
        let
            e = parseTimestampData end
        pure (s, fst <$> e, mrep)
    _otherwise -> mempty
  where
    parseTimestampData :: Org.DateTime -> Maybe (Time.LocalTime, Maybe Repeat)
    parseTimestampData ((y, m, d, _), mtime, mmark1, _) = do
        let
            year = toInteger y
            isLeapYear = Time.isLeapYear year
        dayOfYear <- Time.monthAndDayToDayOfYearValid isLeapYear m d
        localDay <- Time.fromOrdinalDateValid year dayOfYear
        localTimeOfDay <-
            case mtime of
                Nothing -> Just Time.midnight
                Just (hour, minute) -> Time.makeTimeOfDayValid hour minute 0
        let
            rep = mmark1 >>= parseRepeat
        pure (Time.LocalTime {..}, rep)

    parseRepeat :: Org.TimestampMark -> Maybe Repeat
    parseRepeat = \case
        ("+", 1, 'd') -> Just Daily
        ("+", 1, 'x') -> Just Workdays
        ("+", 1, 'w') -> Just Week
        ("+", 1, 'm') -> Just Monthly
        ("+", 1, 'y') -> Just Yearly
        _otherwise -> Nothing
