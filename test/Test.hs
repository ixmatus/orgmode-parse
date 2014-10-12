{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Data.Text            as T

import           Data.Attoparsec.Text
import           Data.OrgMode.Parse
import           Test.HUnit
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ parserHeaderTests ]

parserHeaderTests :: TestTree
parserHeaderTests = testGroup "Parser Heading Tests"
    [ (testCase "Parse Heading Bare"                $ testHeading "* This is a title\n")
    , (testCase "Parse Heading Bare w/ Levels"      $ testHeading "*** This is a title\n")
    , (testCase "Parse Heading w/ Priority"         $ testHeading "* [#A] An important heading\n")
    , (testCase "Parse Heading w/ Priority & State" $ testHeading "* TODO [#A] An important heading with a state indicator\n")
    , (testCase "Parse Heading w/ State"            $ testHeading "* CANCELED An important heading with just state\n")
    , (testCase "Parse Heading w/ Keywords"         $ testHeading "* An important heading :WITH:KEYWORDS:\n")
    , (testCase "Parse Heading Full"                $ testHeading "* DONE [#B] A heading : with [[http://somelink.com][a link]] :WITH:KEYWORDS:\n")
    ]

testHeading :: String -> Assertion
testHeading v = fromEither (parseOnly heading $ T.pack v)

fromEither :: Either String Heading -> Assertion
fromEither (Left e)  = assertBool e  False
fromEither (Right _) = assertBool "" True
