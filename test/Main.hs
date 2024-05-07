module Main (main) where

import Block qualified
import Drawer qualified
import Heading qualified
import Item qualified

import Footnote qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Org tests"
        [ Heading.test
        , Block.test
        , Drawer.test
        , Footnote.test
        , Item.test
        ]
