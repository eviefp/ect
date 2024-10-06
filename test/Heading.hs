module Heading
  ( test
  ) where

import Data.Text (Text)
import Org qualified
import OrgTest qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as TH

data HeadingTest = HeadingTest
  { name :: !String
  , depth :: !Int
  , input :: !Text
  , comment :: !Text
  , expected :: !Org.Heading
  }

test :: TestTree
test =
  testGroup "heading tests"
    . fmap runTest
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
  runTest :: HeadingTest -> TestTree
  runTest HeadingTest {..} =
    TH.testCase name $
      case OrgTest.runParser (Org.parseChildHeading depth) input of
        Left err -> TH.assertFailure err
        Right result -> TH.assertEqual "" expected result
