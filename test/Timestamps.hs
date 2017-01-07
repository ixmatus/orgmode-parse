{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Timestamps where

import           Control.Applicative      ((<*))
import           Control.Monad            (guard)
import           Data.Attoparsec.Text     (endOfLine)
import           Data.HashMap.Strict
import           Data.Monoid              ((<>))
import           Data.OrgMode.Parse
import qualified Data.Text                as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Weekdays                 (weekdays)

import           Data.OrgMode.Parse.Types
import           Util

parserPlanningTests :: TestTree
parserPlanningTests = testGroup "Attoparsec Planning"
    [ (testCase "Parse Planning Schedule" $ testPlanning "SCHEDULED: <2004-02-29 Sun>")
    , (testCase "Parse Planning Deadline" $ testPlanning "DEADLINE: <2004-02-29 Sun>")
    , (testCase "Parse Planning Full"     $ testPlanning "SCHEDULED: <2004-02-29 Sun +1w>")
    , (testCase "Parse Sample Schedule"   $ testPlanningS sExampleStrA sExampleResA)
    ]
  where
    testPlanning  t   = testParser parsePlannings t
    testPlanningS t r = expectParse parsePlannings t (Right r)

(sExampleStrA, sExampleResA) =
  ("SCHEDULED: <2004-02-29 Sun 10:20 +1w -2d>"
   ,(fromList [(SCHEDULED, Timestamp
                                (DateTime
                                 (YMD' (YearMonthDay 2004 2 29))
                                 (Just "Sun")
                                 (Just (10,20))
                                 (Just (Repeater RepeatCumulate 1 UnitWeek))
                                 (Just (Delay DelayAll 2 UnitDay))
                                ) Active Nothing)]))

parserTimestampTests :: TestTree
parserTimestampTests = testGroup "Attoparsec Timestamp"
    [ (testCase "Parse Timestamp Appointment" $ testTimestamp "<2004-02-29 Sun>\n")
    , (testCase "Parse Timestamp Recurring"   $ testTimestamp "<2004-02-29 Sun +1w>\n")
    ]
  where
    testTimestamp t = testParser (parseTimestamp <* endOfLine) t


parserWeekdayTests :: TestTree
parserWeekdayTests = testGroup "Attoparsec Weekday"
  [testCase ("Parse Weekday in " ++ loc) $ mkTest w
  | (loc,ws) <- weekdays, w <- ws, isOrgParsable w]
  where
    mkTest w = expectParse parseTimestamp str (Right res)
      where
        str = "<2004-02-29 " <> w <> " 10:20>"
        res = Timestamp (DateTime
                         (YMD' (YearMonthDay 2004 2 29))
                         (Just w)
                         (Just (10,20))
                         Nothing
                         Nothing) Active Nothing

    dayChars = "]+0123456789>\r\n -" :: String
    isOrgParsable w = T.find (\c -> c `elem` dayChars) w == Nothing
