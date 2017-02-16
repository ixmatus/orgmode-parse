{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Headline where

import           Data.OrgMode.Parse
import           Test.Tasty
import           Test.Tasty.HUnit

import           Util

parserHeadlineTests :: TestTree
parserHeadlineTests = testGroup "Attoparsec Headline"
    [ (testCase "Parse Headline Bare"                $ testHeadline "* This is a title\n")
    , (testCase "Parse Headline Bare with end colon" $ testHeadline "* This heading ends in a colon:")
    , (testCase "Parse Headline Bare w/ Depths"      $ testHeadline "*** This is a title\n")
    , (testCase "Parse Headline w/ Priority"         $ testHeadline "* [#A] An important heading\n")
    , (testCase "Parse Headline w/ Priority & State" $ testHeadline "* TODO [#A] An important heading with a state indicator\n")
    , (testCase "Parse Headline w/ State"            $ testHeadline "* CANCELED An important heading with just state\n")
    , (testCase "Parse Headline w/ Keywords"         $ testHeadline "* An important heading :WITH:KEYWORDS:\n")
    , (testCase "Parse Headline Full"                $ testHeadline "* DONE [#B] A heading : with [[http://somelink.com][a link]] :WITH:KEYWORDS:\n")
    , (testCase "Parse Headline All But Title"       $ testHeadline "* DONE [#A] :WITH:KEYWORDS:\n")
    ]
  where
    testHeadline = testParser (headlineBelowDepth ["TODO","CANCELED","DONE"] Nothing 0)
