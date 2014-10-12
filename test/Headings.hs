{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Headings where

import           Data.OrgMode.Parse
import           Test.Tasty
import           Test.Tasty.HUnit

import           Util

parserHeadingTests :: TestTree
parserHeadingTests = testGroup "Attoparsec Heading"
    [ (testCase "Parse Heading Bare"                $ testHeading "* This is a title\n")
    , (testCase "Parse Heading Bare w/ Levels"      $ testHeading "*** This is a title\n")
    , (testCase "Parse Heading w/ Priority"         $ testHeading "* [#A] An important heading\n")
    , (testCase "Parse Heading w/ Priority & State" $ testHeading "* TODO [#A] An important heading with a state indicator\n")
    , (testCase "Parse Heading w/ State"            $ testHeading "* CANCELED An important heading with just state\n")
    , (testCase "Parse Heading w/ Keywords"         $ testHeading "* An important heading :WITH:KEYWORDS:\n")
    , (testCase "Parse Heading Full"                $ testHeading "* DONE [#B] A heading : with [[http://somelink.com][a link]] :WITH:KEYWORDS:\n")
    ]
  where
    testHeading = testParser (heading)
