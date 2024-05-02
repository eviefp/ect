{-# LANGUAGE TemplateHaskell #-}

module Org
    ( Heading (..)
    , stars
    , keyword
    , priority
    , isComment
    , title
    , tags
    , elements
    , subsections
    , Parser
    , OrgSettings (..)
    , parseChildHeading
    ) where

import Control.Applicative (asum)
import Control.Lens (makeLenses)
import Control.Monad.Reader (Reader)
import Control.Monad.Reader qualified as Reader
import Data.Char qualified as Char
import Data.Functor (void)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as Char
import Prelude hiding (repeat, words)

------------------------------------------------------------
-- Types

-- Elements

data Heading = Heading
    { _stars :: !Int
    , _keyword :: !(Maybe Text)
    , _priority :: !(Maybe Char)
    , _isComment :: !(Maybe Bool)
    , _title :: !(Maybe Text)
    , _tags :: ![Text]
    , _elements :: !()
    , _subsections :: !()
    }
    deriving stock (Eq, Show)

------------------------------------------------------------
-- Lenses

makeLenses ''Heading

------------------------------------------------------------
-- Parser
data OrgSettings = OrgSettings
    { _orgTodoKeywords :: ![Text]
    , _later :: !Bool
    }

type Parser = M.ParsecT Void Text (Reader OrgSettings)

parseChildHeading :: Int -> Parser Heading
parseChildHeading depth = do
    _stars <- T.length <$> someOf '*'
    if _stars <= depth
        then M.failure Nothing Set.empty
        else do
            state <- Reader.ask
            _keyword <-
                M.optional . M.try $
                    spaces *> (asum . fmap M.chunk $ state._orgTodoKeywords)
            _priority <- M.optional . M.try $ spaces *> parsePriority
            _isComment <- fmap (fmap (const True)) . M.optional . M.try $ spaces *> Char.string "COMMENT"
            rest <- M.optional . M.try $ spaces *> M.takeWhile1P Nothing (/= '\n')
            let
                (_title, _tags) = parseRest rest
            let
                _elements = ()
                _subsections = ()
            pure Heading {..}
  where
    parsePriority :: Parser Char
    parsePriority = do
        _ <- Char.string "[#"
        prio <- M.satisfy (\c -> Char.isAsciiUpper c || Char.isDigit c)
        _ <- Char.char ']'
        pure prio

    parseRest :: Maybe Text -> (Maybe Text, [Text])
    parseRest =
        \case
            Nothing -> (Nothing, [])
            Just rest ->
                let
                    words = T.words rest
                in
                    case reverse words of
                        (maybeTags : rest') ->
                            case ( T.uncons maybeTags
                                 , T.unsnoc maybeTags
                                 , T.all (\c -> Char.isAlphaNum c || elem @[] c ":_@#%") maybeTags
                                 ) of
                                (Just (':', _), Just (_, ':'), True) -> (Just . T.unwords . reverse $ rest', filter (/= "") $ T.splitOn ":" maybeTags)
                                _otherwise -> (Just rest, [])
                        _otherwise -> (Nothing, [])

someOf :: Char -> Parser Text
someOf c = M.takeWhile1P (Just $ "some of " <> [c]) (== c)

spaces :: Parser ()
spaces = void $ M.takeWhile1P Nothing (== ' ')
