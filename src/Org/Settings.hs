{-# LANGUAGE TemplateHaskell #-}

module Org.Settings
  ( OrgSettings (..)
  , todoKeywords
  , todo
  , Parser
  , identifier
  , someOf
  , spaces
  ) where

import Control.Lens (makeLenses)
import Control.Monad.Reader (Reader)
import Data.Char qualified as Char
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec qualified as M

data OrgSettings = OrgSettings
  { _todoKeywords :: ![Text]
  , _todo :: !()
  }

makeLenses ''OrgSettings

type Parser = M.ParsecT Void Text (Reader OrgSettings)

someOf :: Char -> Parser Text
someOf c = M.takeWhile1P (Just $ "some of " <> [c]) (== c)

spaces :: Parser ()
spaces = void $ M.takeWhile1P Nothing (== ' ')

identifier :: Parser Text
identifier = M.takeWhile1P Nothing (\c -> Char.isAlphaNum c || c == '-' || c == '_')
