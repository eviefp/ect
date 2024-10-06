module Block
  ( test
  ) where

import Data.Text (Text)
import Org qualified
import OrgTest qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as TH

data BlockTest = BlockTest
  { name :: !String
  , input :: !Text
  , skip :: !Bool
  , expected :: !Org.Block
  }

test :: TestTree
test =
  testGroup "block tests"
    . fmap runTest
    $ [ BlockTest
          { name = "simple block"
          , input = "#+begin_foo\n#+end_foo\n"
          , skip = False
          , expected =
              Org.Block
                { Org._name = "foo"
                , Org._parameters = Nothing
                , Org._content = Nothing
                }
          }
      , BlockTest
          { name = "simple block with params"
          , input = "#+begin_foo bar\n#+end_foo\n"
          , skip = False
          , expected =
              Org.Block
                { Org._name = "foo"
                , Org._parameters = Just "bar"
                , Org._content = Nothing
                }
          }
      , BlockTest
          { name = "block with content"
          , input = "#+begin_foo bar\ncontent\n#+end_foo\n"
          , skip = True
          , expected =
              Org.Block
                { Org._name = "foo"
                , Org._parameters = Just "bar"
                , Org._content = Nothing
                }
          }
      ]
 where
  runTest :: BlockTest -> TestTree
  runTest BlockTest {..} =
    TH.testCase (mkName skip name) $
      if skip
        then TH.assertBool "" True
        else case OrgTest.runParser Org.parseBlock input of
          Left err -> TH.assertFailure err
          Right result -> TH.assertEqual "" expected result

  mkName :: Bool -> String -> String
  mkName True = (<>) "[Skip] "
  mkName False = id
