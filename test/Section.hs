{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Section (parserSectionTests) where

import           Data.OrgMode.Parse
import qualified Data.Text as T
import           Test.Tasty
import           Test.Tasty.HUnit

import           Util

parserSectionTests :: TestTree
parserSectionTests = testGroup "Attoparsec Section"
    [ (testCase "Parse Section with clock drawer"
       $ testClock (Just "LOGBOOK") clockDrawer)
    , (testCase "Parse Section without clock drawer"
       $ testClock Nothing clockNoDrawer)
    ]
  where
    clockDrawer = T.intercalate "\n" [ ":LOGBOOK:"
                                     , "CLOCK: [2017-02-08 Wed 13:40]--[2017-02-08 Wed 13:50] =>  0:10"
                                     , ":END:"
                                     ]
    clockNoDrawer = "CLOCK: [2017-02-08 Wed 13:40]--[2017-02-08 Wed 13:50] =>  0:10"
    testClock mDrawer txt = expectParse (parseSection mDrawer) txt (Right clockSection)

clockSection = Section { sectionPlannings  = Plns mempty
                       , sectionClocks     = [clockEntry]
                       , sectionProperties = mempty
                       , sectionParagraph  = ""
                       }
  where
    clockEntry = (Just ts, Just (0,10))
    dt x = DateTime { yearMonthDay = YMD' (YearMonthDay 2017 2 8)
                    , dayName      = Just "Wed"
                    , hourMinute   = Just (13,x)
                    , repeater     = Nothing
                    , delay        = Nothing
                    }
    ts = Timestamp { tsTime    = dt 40
                   , tsActive  = Inactive
                   , tsEndTime = Just (dt 50)
                   }
