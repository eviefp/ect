module Drawer
  ( test
  ) where

import Data.Text (Text)
import Org qualified
import OrgTest qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as TH

data DrawerTest = DrawerTest
  { name :: !String
  , input :: !Text
  , skip :: !Bool
  , expected :: !Org.Drawer
  }

test :: TestTree
test =
  testGroup "drawer tests"
    . fmap runTest
    $ [ DrawerTest
          { name = "simple drawer"
          , input = ":foo:\n:end:\n"
          , skip = False
          , expected =
              Org.Drawer
                { Org._name = "foo"
                , Org._content = Nothing
                }
          }
      , DrawerTest
          { name = "drawer with content"
          , input = ":foo:\ncontent\n:end:\n"
          , skip = True
          , expected =
              Org.Drawer
                { Org._name = "foo"
                , Org._content = Just "content"
                }
          }
      ]
 where
  runTest :: DrawerTest -> TestTree
  runTest DrawerTest {..} =
    TH.testCase (mkName skip name) $
      if skip
        then TH.assertBool "" True
        else case OrgTest.runParser Org.parseDrawer input of
          Left err -> TH.assertFailure err
          Right result -> TH.assertEqual "" expected result

  mkName :: Bool -> String -> String
  mkName True = (<>) "[Skip] "
  mkName False = id
