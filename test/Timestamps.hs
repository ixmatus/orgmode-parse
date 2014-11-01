{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Timestamps where

import           Control.Applicative  ((<*))
import           Data.Attoparsec.Text (endOfLine)
import           Data.OrgMode.Parse
import           Test.Tasty
import           Test.Tasty.HUnit

import           Util

parserTimestampTests :: TestTree
parserTimestampTests = testGroup "Attoparsec Timestamp"
    [ (testCase "Parse Timestamp Schedule"          $ testTimestamp "SCHEDULED: <2004-02-29 Sun>\n")
    , (testCase "Parse Timestamp Deadline"          $ testTimestamp "DEADLINE: <2004-02-29 Sun>\n")
    , (testCase "Parse Timestamp Appointment"       $ testTimestamp "<2004-02-29 Sun>\n")
    , (testCase "Parse Timestamp Recurring"         $ testTimestamp "<2004-02-29 Sun +1w>\n")
    , (testCase "Parse Timestamp Full"              $ testTimestamp "SCHEDULED: <2004-02-29 Sun +1w>\n")
    ]
  where
    testTimestamp t = testParser (parseTimestamp (Open '<') (Close '>') <* endOfLine) t
