{-# LANGUAGE TemplateHaskell #-}

module Org.Heading
    ( Heading (..)
    , stars
    , keyword
    , priority
    , isComment
    , title
    , tags
    , elements
    , subsections
    , parseChildHeading
    ) where

import Control.Applicative (asum)
import Control.Lens (makeLenses)
import Control.Monad.Reader qualified as Reader
import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Org.Settings (Parser)
import Org.Settings qualified as Parser
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as Char
import Prelude hiding (repeat, words)

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

makeLenses ''Heading

parseChildHeading :: Int -> Parser Heading
parseChildHeading depth = do
    _stars <- T.length <$> Parser.someOf '*'
    if _stars <= depth
        then M.failure Nothing Set.empty
        else do
            state <- Reader.ask
            _keyword <-
                M.optional . M.try $
                    Parser.spaces *> (asum . fmap M.chunk $ state._todoKeywords)
            _priority <- M.optional . M.try $ Parser.spaces *> parsePriority
            _isComment <- fmap (fmap (const True)) . M.optional . M.try $ Parser.spaces *> Char.string "COMMENT"
            rest <- M.optional . M.try $ Parser.spaces *> M.takeWhile1P Nothing (/= '\n')
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
