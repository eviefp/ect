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
    , Properties (..)
    , propertyUid
    , propertyClass
    , propertyCreated
    , propertyLastModified
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
    ) where

import Control.Lens (makeLenses)
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

import Config qualified
import Org.Parser qualified as OrgParser
import Org.Types qualified as Org
import Org.Walk qualified as Org

data Properties = Properties
    { _propertyUid :: !Text
    , _propertyClass :: !Text
    , _propertyCreated :: !(Maybe Text) -- TODO: parse
    , _propertyLastModified :: !(Maybe Text)
    , _propertyLocation :: !(Maybe Text)
    , _propertyOrganizer :: !(Maybe Text)
    , _propertySequence :: !Int
    , _propertyStatus :: !(Maybe Text)
    , _propertyTransparency :: !Text
    }
    deriving stock (Eq, Generic)

makeLenses ''Properties

data Entry = Entry
    { _entryTitle :: !Text
    , _entryTags :: ![Text]
    , _entryStartTime :: !Time.LocalTime
    , _entryEndTime :: !(Maybe Time.LocalTime)
    , _entryDescription :: !Text
    , _entryProperties :: !Properties
    , _entrySection :: !(Maybe Org.OrgSection)
    }
    deriving stock (Eq, Generic)

makeLenses ''Entry

newtype Calendar = Calendar {calendarEntries :: [Entry]}

mkCalendar :: (MonadIO m) => [FilePath] -> m Calendar
mkCalendar files =
    Calendar
        . List.sortOn _entryStartTime
        . concatMap walkEntries
        <$> traverse (OrgParser.parseOrgDocIO OrgParser.defaultOrgOptions) files

fromConfig :: (MonadIO m) => Config.EctConfig -> m Calendar
fromConfig config =
    let
        calendars = T.unpack . Config.path <$> config.calendars
    in
        mkCalendar calendars

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
    in
        Entry {..}

emptyProperties :: Properties
emptyProperties =
    let
        _propertyUid = ""
        _propertyClass = ""
        _propertyCreated = Nothing
        _propertyLastModified = Nothing
        _propertyLocation = Nothing
        _propertyOrganizer = Nothing
        _propertySequence = 0
        _propertyStatus = Nothing
        _propertyTransparency = ""
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
printUpcomingDate = Time.formatTime Time.defaultTimeLocale "%A, %d %b"

walkEntries :: Org.OrgDocument -> [Entry]
walkEntries = Org.query go -- TODO: does this go deep?
  where
    go :: Org.OrgSection -> [Entry]
    go section =
        case getFirst $ foldMap (Org.query findDatesInParagraphs) (Org.sectionChildren section) of
            Nothing -> []
            Just (startDate, mEndDate) ->
                let
                    _entryTitle = Org.sectionRawTitle section
                    _entryTags = Org.sectionTags section
                    _entryStartTime = startDate
                    _entryEndTime = mEndDate
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

        _propertyUid = getDef "UID" ""
        _propertyClass = getDef "Class" ""
        _propertyCreated = get "Created"
        _propertyLastModified = get "LastModified"
        _propertyLocation = get "Location"
        _propertyOrganizer = get "Organizer"
        _propertySequence = read . T.unpack $ getDef "Sequence" "0"
        _propertyStatus = get "Status"
        _propertyTransparency = getDef "Transparency" ""
    in
        Properties {..}

findDatesInParagraphs :: Org.OrgObject -> First (Time.LocalTime, Maybe Time.LocalTime)
findDatesInParagraphs = \case
    Org.Timestamp (Org.TimestampData _ start) -> First $ (,Nothing) <$> parseTimestampData start
    Org.Timestamp (Org.TimestampRange _ start end) -> First $ (,parseTimestampData end) <$> parseTimestampData start
    _otherwise -> mempty
  where
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
