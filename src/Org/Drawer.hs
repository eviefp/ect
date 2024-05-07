{-# LANGUAGE TemplateHaskell #-}

module Org.Drawer
    ( Drawer (..)
    , name
    , content
    , parseDrawer
    ) where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Org.Settings (Parser)
import Org.Settings qualified as Org
import Text.Megaparsec.Char qualified as Char
import Prelude hiding (repeat, words)

{- | 4.2.1 & 4.3.1 (Greater and Lesser) blocks
Technically, the _content can include any other elements for a Greater Block, but not so for Lesser.

We're conflating the two because the difference doesn't seem significant if we're just parsing or
printing.

TODO: make it such that _content is actually whatever type is for formatted text.
-}
data Drawer = Drawer
    { _name :: !Text
    , _content :: !(Maybe Text)
    }
    deriving stock (Eq, Show)

------------------------------------------------------------
-- Lenses

makeLenses ''Drawer

parseDrawer :: Parser Drawer
parseDrawer = do
    _ <- Char.string' ":"
    _name <- Org.identifier
    _ <- Char.string' ":"
    _ <- Char.newline
    _content <- parseRest
    _ <- Char.string' ":end:"
    _ <- Char.newline
    pure Drawer {..}
  where
    parseRest :: Parser (Maybe Text)
    parseRest = pure Nothing
