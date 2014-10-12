{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module PropertyDrawer where

import           Data.OrgMode.Parse
import           Test.Tasty
import           Test.Tasty.HUnit

import           Util

parserPropertyDrawerTests :: TestTree
parserPropertyDrawerTests = testGroup "Attoparsec PropertyDrawer"
    [ (testCase "Parse Empty Drawer"    $ testProps emptyDrawer)
    , (testCase "Parse Drawer w/ Props" $ testProps drawerWKeys)
    ]
  where
    emptyDrawer = ":PROPERTIES:\n:END:\n"
    drawerWKeys = ":PROPERTIES:\n    :URL: http://someurl.com?query\n :notes: you should be taking them\n:END:\n"
    testProps = testParser (drawer)
