module OrgTest
    ( runParser
    ) where

import Control.Monad.Trans.Reader qualified as Reader
import Data.Bifunctor qualified as Bifunctor
import Data.Text (Text)
import Org qualified
import Text.Megaparsec qualified as Parser

defaultSettings :: Org.OrgSettings
defaultSettings =
    Org.OrgSettings
        { Org._todoKeywords = ["TODO"]
        , Org._todo = ()
        }

runParser :: Org.Parser a -> Text -> Either String a
runParser p =
    Bifunctor.first Parser.errorBundlePretty
        . (`Reader.runReader` defaultSettings)
        . Parser.runParserT p "testing"
