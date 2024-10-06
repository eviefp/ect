module Footnote
  ( test
  ) where

import Data.Text (Text)
import Org qualified
import OrgTest qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as TH

data FootnoteTest = FootnoteTest
  { name :: !String
  , input :: !Text
  , skip :: !Bool
  , expected :: !Org.Footnote
  }

test :: TestTree
test =
  testGroup "footnote tests"
    . fmap runTest
    $ [ FootnoteTest
          { name = "simple foontote"
          , input = "[fn:foo] "
          , skip = False
          , expected =
              Org.Footnote
                { Org._label = "foo"
                , Org._content = Nothing
                }
          }
      , FootnoteTest
          { name = "simple footnote with content"
          , input = "[fn:foo] some footnote\nblah\n\n"
          , skip = True
          , expected =
              Org.Footnote
                { Org._label = "foo"
                , Org._content = Nothing
                }
          }
      ]
 where
  runTest :: FootnoteTest -> TestTree
  runTest FootnoteTest {..} =
    TH.testCase (mkName skip name) $
      if skip
        then TH.assertBool "" True
        else case OrgTest.runParser Org.parseFootnote input of
          Left err -> TH.assertFailure err
          Right result -> TH.assertEqual "" expected result

  mkName :: Bool -> String -> String
  mkName True = (<>) "[Skip] "
  mkName False = id
