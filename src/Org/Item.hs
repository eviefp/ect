{-# LANGUAGE TemplateHaskell #-}

module Org.Item
  ( Bullet (..)
  , _Asterix
  , _Hyphen
  , _Plus
  , Checkbox (..)
  , _Unchecked
  , _Partial
  , _Completed
  , Item (..)
  , indent
  , kind
  , counter
  , checkbox
  , tag
  , content
  , subLists
  , parseItem
  , List (..)
  , listIndent
  , items
  , parseList
  ) where

import Control.Applicative (asum)
import Control.Lens (makeLenses, makePrisms)
import Data.Char qualified as Char
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as Text
import Org.Settings (Parser)
import Org.Settings qualified as Parser
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as Char
import Prelude hiding (repeat, words)

data Bullet = Asterix | Hyphen | Plus
  deriving stock (Eq, Show)

data Checkbox = Unchecked | Partial | Completed
  deriving stock (Eq, Show)

-- | 4.2.6 Item
data Item = Item
  { _indent :: !Int
  , _kind :: !Bullet
  , _counter :: !(Maybe Char)
  , _checkbox :: !(Maybe Checkbox)
  , _tag :: !(Maybe Text)
  , _content :: !(Maybe Text)
  , _subLists :: ![List]
  }
  deriving stock (Eq, Show)

-- | 4.2.7 List
data List = List
  { _listIndent :: !Int
  , _items :: !(NonEmpty Item)
  }
  deriving stock (Eq, Show)

------------------------------------------------------------
-- Lenses
makePrisms ''Bullet

makePrisms ''Checkbox

makeLenses ''Item

makeLenses ''List

------------------------------------------------------------
-- Parser

-- | Note that for tags, we take the first occurance of " :: "
-- rather than the last.
parseItem :: Parser Item
parseItem = do
  _indent <- Text.length <$> M.takeWhileP Nothing (== ' ')
  _kind <- parseBullet _indent
  _counter <- M.optional . M.try $ Parser.spaces *> parsePriority
  _checkbox <- M.optional . M.try $ Parser.spaces *> parseCheckbox
  _tag <- M.optional . M.try $ Parser.spaces *> parseTag ""
  _content <- M.optional . M.try $ Parser.spaces *> parseContent
  _subLists <- M.many . M.try $ Char.char '\n' *> parseList (_indent + 1)
  pure Item {..}
 where
  parsePriority :: Parser Char
  parsePriority = do
    _ <- Char.string "[#"
    prio <- M.satisfy (\c -> Char.isAsciiUpper c || Char.isDigit c)
    _ <- Char.char ']'
    pure prio

  parseBullet :: Int -> Parser Bullet
  parseBullet i =
    asum $
      [ Char.char '-' $> Hyphen
      , Char.char '+' $> Plus
      ]
        <> [Char.char '*' $> Asterix | i /= 0]

  parseCheckbox :: Parser Checkbox
  parseCheckbox = do
    _ <- Char.char '['
    result <-
      asum
        [ Char.char ' ' $> Unchecked
        , Char.char '-' $> Partial
        , Char.char 'X' $> Completed
        ]
    _ <- Char.char ']'
    pure result

  parseTag :: Text -> Parser Text
  parseTag start = do
    rest <- M.takeWhile1P Nothing (\c -> c /= '\n' && c /= ' ')
    M.optional (Char.string " :: ") >>= \case
      Just _ -> pure $ start <> rest
      Nothing ->
        M.optional (Char.char '\n') >>= \case
          Nothing -> do
            sp <- M.takeWhile1P Nothing (== ' ')
            parseTag (start <> rest <> sp)
          Just _ -> M.parseError mempty

  -- Temporary parse everything until newline
  parseContent :: Parser Text
  parseContent =
    M.takeWhile1P Nothing (/= '\n')

parseList :: Int -> Parser List
parseList minIndent = do
  first <- parseItem
  if first._indent < minIndent
    then M.parseError mempty
    else do
      rest <- go first._indent
      let
        _listIndent = first._indent
        _items = first :| rest
      pure List {..}
 where
  go :: Int -> Parser [Item]
  go i = do
    result <- M.optional $ M.try do
      next <- Char.char '\n' *> parseItem
      if next._indent == i
        then (next :) <$> go i
        else M.parseError mempty
    case result of
      Just r -> pure r
      Nothing -> pure []
