{-# LANGUAGE TemplateHaskell #-}

module Org.Footnote
    ( Footnote (..)
    , label
    , content
    , parseFootnote
    ) where

import Control.Lens (makeLenses)
import Data.Text (Text)
import Org.Settings (Parser)
import Org.Settings qualified as Org
import Text.Megaparsec.Char qualified as Char
import Prelude hiding (repeat, words)

-- | 4.2.4 Footnotes
data Footnote = Footnote
    { _label :: !Text
    , _content :: !(Maybe Text)
    }
    deriving stock (Eq, Show)

------------------------------------------------------------
-- Lenses

makeLenses ''Footnote

parseFootnote :: Parser Footnote
parseFootnote = do
    _ <- Char.string' "[fn:"
    _label <- Org.identifier
    _ <- Char.string "]"
    Org.spaces

    _content <- parseRest
    pure Footnote {..}
  where
    parseRest :: Parser (Maybe Text)
    parseRest = pure Nothing
