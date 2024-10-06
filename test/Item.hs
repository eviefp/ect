module Item
  ( test
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Org qualified
import OrgTest qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as TH

test :: TestTree
test = testGroup "Lists and items" [itemTests, listTests]

data ItemTest = ItemTest
  { name :: !String
  , input :: !Text
  , skip :: !Bool
  , expected :: !Org.Item
  }

itemTests :: TestTree
itemTests =
  testGroup "item tests"
    . fmap runTest
    $ [ ItemTest
          { name = "empty item"
          , input = "- "
          , skip = False
          , expected =
              Org.Item
                { Org._indent = 0
                , Org._kind = Org.Hyphen
                , Org._counter = Nothing
                , Org._checkbox = Nothing
                , Org._tag = Nothing
                , Org._content = Nothing
                , Org._subLists = []
                }
          }
      , ItemTest
          { name = "item with counter"
          , input = "+ [#3]"
          , skip = False
          , expected =
              Org.Item
                { Org._indent = 0
                , Org._kind = Org.Plus
                , Org._counter = Just '3'
                , Org._checkbox = Nothing
                , Org._tag = Nothing
                , Org._content = Nothing
                , Org._subLists = []
                }
          }
      , ItemTest
          { name = "simple item with tag"
          , input = "- tag : here :: "
          , skip = False
          , expected =
              Org.Item
                { Org._indent = 0
                , Org._kind = Org.Hyphen
                , Org._counter = Nothing
                , Org._checkbox = Nothing
                , Org._tag = Just "tag : here"
                , Org._content = Nothing
                , Org._subLists = []
                }
          }
      , ItemTest
          { name = "item with checkbox"
          , input = "- [ ]"
          , skip = False
          , expected =
              Org.Item
                { Org._indent = 0
                , Org._kind = Org.Hyphen
                , Org._counter = Nothing
                , Org._checkbox = Just Org.Unchecked
                , Org._tag = Nothing
                , Org._content = Nothing
                , Org._subLists = []
                }
          }
      , ItemTest
          { name = "item with counter and checkbox"
          , input = "- [#A] [-]"
          , skip = False
          , expected =
              Org.Item
                { Org._indent = 0
                , Org._kind = Org.Hyphen
                , Org._counter = Just 'A'
                , Org._checkbox = Just Org.Partial
                , Org._tag = Nothing
                , Org._content = Nothing
                , Org._subLists = []
                }
          }
      , ItemTest
          { name = "item counter/compl checkbox"
          , input = "- [#A] [X]"
          , skip = False
          , expected =
              Org.Item
                { Org._indent = 0
                , Org._kind = Org.Hyphen
                , Org._counter = Just 'A'
                , Org._checkbox = Just Org.Completed
                , Org._tag = Nothing
                , Org._content = Nothing
                , Org._subLists = []
                }
          }
      , ItemTest
          { name = "item with tag"
          , input = "- [-] tag : here :: content"
          , skip = False
          , expected =
              Org.Item
                { Org._indent = 0
                , Org._kind = Org.Hyphen
                , Org._counter = Nothing
                , Org._checkbox = Just Org.Partial
                , Org._tag = Just "tag : here"
                , Org._content = Nothing
                , Org._subLists = []
                }
          }
      ]
 where
  runTest :: ItemTest -> TestTree
  runTest ItemTest {..} =
    TH.testCase (mkName skip name) $
      if skip
        then TH.assertBool "" True
        else case OrgTest.runParser Org.parseItem input of
          Left err -> TH.assertFailure err
          Right result -> TH.assertEqual "" expected result

  mkName :: Bool -> String -> String
  mkName True = (<>) "[Skip] "
  mkName False = id

data ListTest = ListTest
  { name :: !String
  , input :: !Text
  , skip :: !Bool
  , expected :: !Org.List
  }

listTests :: TestTree
listTests =
  testGroup "list tests"
    . fmap runTest
    $ [ ListTest
          { name = "one element list"
          , input = "- first :: "
          , skip = False
          , expected =
              mkList 0 . pure $ mkSimpleItem 0 "first" []
          }
      , ListTest
          { name = "three element list"
          , input = "- first :: \n- second :: \n- third :: "
          , skip = False
          , expected =
              mkList 0 $
                mkSimpleItem 0 "first" []
                  :| [ mkSimpleItem 0 "second" []
                     , mkSimpleItem 0 "third" []
                     ]
          }
      , ListTest
          { name = "simple sublist"
          , input = "- first :: \n  - sublist :: \n- second :: "
          , skip = False
          , expected =
              mkList 0 $
                mkSimpleItem
                  0
                  "first"
                  [mkList 2 $ pure $ mkSimpleItem 2 "sublist" []]
                  :| [mkSimpleItem 0 "second" []]
          }
      , ListTest
          { name = "sub-sublist"
          , input = "- 1 :: \n  - 1.1 :: \n  - 1.2 :: \n    - 1.2.1 :: \n- 2 :: "
          , skip = False
          , expected =
              mkList 0 $
                mkSimpleItem
                  0
                  "1"
                  [ mkList 2 $
                      mkSimpleItem 2 "1.1" []
                        :| [ mkSimpleItem
                              2
                              "1.2"
                              [mkList 4 $ pure $ mkSimpleItem 4 "1.2.1" []]
                           ]
                  ]
                  :| [ mkSimpleItem 0 "2" []
                     ]
          }
      ]
 where
  runTest :: ListTest -> TestTree
  runTest ListTest {..} =
    TH.testCase (mkName skip name) $
      if skip
        then TH.assertBool "" True
        else case OrgTest.runParser (Org.parseList 0) input of
          Left err -> TH.assertFailure err
          Right result -> TH.assertEqual "" expected result

  mkName :: Bool -> String -> String
  mkName True = (<>) "[Skip] "
  mkName False = id

mkList :: Int -> NonEmpty Org.Item -> Org.List
mkList _listIndent _items = Org.List {..}

mkSimpleItem :: Int -> Text -> [Org.List] -> Org.Item
mkSimpleItem indent tag sub =
  Org.Item
    { Org._indent = indent
    , Org._kind = Org.Hyphen
    , Org._counter = Nothing
    , Org._checkbox = Nothing
    , Org._tag = Just tag
    , Org._content = Nothing
    , Org._subLists = sub
    }
