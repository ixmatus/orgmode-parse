{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Timestamps where

import           Control.Applicative      ((<*))
import           Data.Attoparsec.Text     (endOfLine)
import           Data.OrgMode.Parse
import           Data.OrgMode.Parse.Types
import           Test.Tasty
import           Test.Tasty.HUnit

import           Util

parserPlanningTests :: TestTree
parserPlanningTests = testGroup "Attoparsec Planning"
    [ (testCase "Parse Planning Schedule" $ testPlanning "SCHEDULED: <2004-02-29 Sun>")
    , (testCase "Parse Planning Deadline" $ testPlanning "DEADLINE: <2004-02-29 Sun>")
    , (testCase "Parse Planning Full"     $ testPlanning "SCHEDULED: <2004-02-29 Sun +1w>")
    ]
  where
    testPlanning t = testParser parsePlannings t

parserTimestampTests :: TestTree
parserTimestampTests = testGroup "Attoparsec Timestamp"
    [ (testCase "Parse Timestamp Appointment" $ testTimestamp "<2004-02-29 Sun>\n")
    , (testCase "Parse Timestamp Recurring"   $ testTimestamp "<2004-02-29 Sun +1w>\n")
    ]
  where
    testTimestamp t = testParser (parseTimestamp <* endOfLine) t
