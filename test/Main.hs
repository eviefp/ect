module Main (main) where

import Control.Monad.Trans.Reader qualified as Reader
import Data.Bifunctor qualified as Bifunctor
import Data.Text (Text)
import Org qualified
import Test.HUnit qualified as Test
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit qualified as TH
import Text.Megaparsec qualified as Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [headingTests]

-------------------------------------------------------
--

runParser :: Org.Parser a -> Org.OrgSettings -> Text -> Either String a
runParser p settings =
    Bifunctor.first Parser.errorBundlePretty
        . (`Reader.runReader` settings)
        . Parser.runParserT p "testing"

defaultSettings :: Org.OrgSettings
defaultSettings =
    Org.OrgSettings
        { Org._orgTodoKeywords = ["TODO"]
        , Org._later = False
        }

data HeadingTest = HeadingTest
    { name :: !String
    , depth :: !Int
    , input :: !Text
    , comment :: !Text
    , expected :: !Org.Heading
    }

headingTests :: TestTree
headingTests =
    testGroup "heading tests"
        . fmap test
        $ [ HeadingTest
                { name = "just stars"
                , depth = 0
                , input = "*\n"
                , comment = "This should actually fail because * needs a space after."
                , expected =
                    Org.Heading
                        { Org._stars = 1
                        , Org._keyword = Nothing
                        , Org._priority = Nothing
                        , Org._isComment = Nothing
                        , Org._title = Nothing
                        , Org._tags = []
                        , Org._elements = ()
                        , Org._subsections = ()
                        }
                }
          , HeadingTest
                { name = "star todo title"
                , depth = 0
                , input = "* TODO Some title goes here :tag1:\n"
                , comment = ""
                , expected =
                    Org.Heading
                        { Org._stars = 1
                        , Org._keyword = Just "TODO"
                        , Org._priority = Nothing
                        , Org._isComment = Nothing
                        , Org._title = Just "Some title goes here"
                        , Org._tags = ["tag1"]
                        , Org._elements = ()
                        , Org._subsections = ()
                        }
                }
          , HeadingTest
                { name = "title with colon"
                , depth = 0
                , input = "* Title: with colon\n"
                , comment = ""
                , expected =
                    Org.Heading
                        { Org._stars = 1
                        , Org._keyword = Nothing
                        , Org._priority = Nothing
                        , Org._isComment = Nothing
                        , Org._title = Just "Title: with colon"
                        , Org._tags = []
                        , Org._elements = ()
                        , Org._subsections = ()
                        }
                }
          , HeadingTest
                { name = "title with fake tags"
                , depth = 0
                , input = "* Title: :with:some:fake:tag: colon\n"
                , comment = ""
                , expected =
                    Org.Heading
                        { Org._stars = 1
                        , Org._keyword = Nothing
                        , Org._priority = Nothing
                        , Org._isComment = Nothing
                        , Org._title = Just "Title: :with:some:fake:tag: colon"
                        , Org._tags = []
                        , Org._elements = ()
                        , Org._subsections = ()
                        }
                }
          , HeadingTest
                { name = "title with fake and real tags"
                , depth = 0
                , input = "* Title: :with:some:fake:tag: colon :tag1:tag2:\n"
                , comment = ""
                , expected =
                    Org.Heading
                        { Org._stars = 1
                        , Org._keyword = Nothing
                        , Org._priority = Nothing
                        , Org._isComment = Nothing
                        , Org._title = Just "Title: :with:some:fake:tag: colon"
                        , Org._tags = ["tag1", "tag2"]
                        , Org._elements = ()
                        , Org._subsections = ()
                        }
                }
          , HeadingTest
                { name = "everything"
                , depth = 0
                , input = "* TODO [#A] COMMENT Some title goes here :tag1:tag2:\n"
                , comment = ""
                , expected =
                    Org.Heading
                        { Org._stars = 1
                        , Org._keyword = Just "TODO"
                        , Org._priority = Just 'A'
                        , Org._isComment = Just True
                        , Org._title = Just "Some title goes here"
                        , Org._tags = ["tag1", "tag2"]
                        , Org._elements = ()
                        , Org._subsections = ()
                        }
                }
          ]
  where
    test :: HeadingTest -> TestTree
    test HeadingTest {..} =
        TH.testCase name $
            case runParser (Org.parseChildHeading depth) defaultSettings input of
                Left err -> Test.assertFailure err
                Right result -> Test.assertEqual "" expected result
