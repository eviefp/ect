{-# LANGUAGE TemplateHaskell #-}

module Org.Block
  ( Block (..)
  , name
  , parameters
  , content
  , parseBlock
  ) where

import Control.Lens (makeLenses)
import Data.Char qualified as Char
import Data.Text (Text)
import Org.Settings (Parser)
import Org.Settings qualified as Parser
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as Char
import Prelude hiding (repeat, words)

-- | 4.2.1 & 4.3.1 (Greater and Lesser) blocks
-- Technically, the _content can include any other elements for a Greater Block, but not so for Lesser.
--
-- We're conflating the two because the difference doesn't seem significant if we're just parsing or
-- printing.
--
-- TODO: make it such that _content is actually whatever type is for formatted text.
data Block = Block
  { _name :: !Text
  , _parameters :: !(Maybe Text)
  , _content :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

------------------------------------------------------------
-- Lenses

makeLenses ''Block

parseBlock :: Parser Block
parseBlock = do
  _ <- Char.string' "#+begin_"
  _name <- M.takeWhile1P Nothing (not . Char.isSpace)
  _parameters <- M.optional $ Parser.spaces *> M.takeWhileP Nothing (/= '\n')
  _ <- Char.newline
  _content <- parseRest
  _ <- Char.string' $ "#+end_" <> _name
  _ <- Char.newline
  pure Block {..}
 where
  parseRest :: Parser (Maybe Text)
  parseRest = pure Nothing
