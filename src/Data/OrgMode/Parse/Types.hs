-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Types
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Types and utility functions.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Data.OrgMode.Parse.Types
( Document (..)
, Section (..)
, Heading  (..)
, Priority (..)
, TodoKeyword  (..)
, Duration
, Timestamp    (..)
, DateTime (..)
, TimeUnit (..)
, RepeaterType (..)
, Repeater (..)
, DelayType (..)
, Delay (..)
) where

import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)
import           Data.Thyme.Calendar  (YearMonthDay(..))
import           Data.Thyme.LocalTime (LocalTime (..), Hour(..),Minute(..))
import           Data.Tree            (Forest(..),Tree(..))
import           GHC.Generics

----------------------------------------------------------------------------
data Document = Document {
    openingText :: Text
  , topHeadings :: [Heading]
  } deriving (Show, Eq, Generic)

data Section = Section {
      sectionPlannings  :: HashMap PlanningKeyword Timestamp
    , sectionProperties :: HashMap Text         Text
    , sectionClocks     :: [(Maybe Timestamp, Maybe Duration)]
    , sectionParagraph  :: Text
  } deriving (Show, Eq, Generic)

data Heading = Heading
    { level       :: Int                --
    , keyword     :: Maybe TodoKeyword  --
    , priority    :: Maybe Priority     -- Header line
    , title       :: Text               -- properties
    , stats       :: Maybe Stats        --
    , tags        :: [Text]             --
    , section     :: Section            -- Next-line
    , subHeadings :: [Heading]          -- elements
    } deriving (Show, Eq, Generic)


data Priority = A | B | C
  deriving (Show, Read, Eq, Ord, Generic)

data TodoKeyword = TODO
                 | DONE
                 | OtherKeyword Text
  deriving (Show, Eq, Generic)


data Stats = StatsPtc Int
           | StatsOf  Int Int
           deriving (Show, Eq, Generic)


type Duration = (Hour,Minute)

data PlanningKeyword = SCHEDULED | DEADLINE | CLOSED
  deriving (Show, Eq, Generic)

-- -- This might be the form to use if we were supporting <diary> timestamps
-- data Timestamp = Dairy Text
--                | Time  TimestampTime
--               deriving (Show, Eq, Generic)

data Timestamp = Timestamp {
    tsTime    :: DateTime
  , tsActive  :: Bool
  , tsEndTime :: Maybe DateTime
  } deriving (Show, Eq, Generic)

data DateTime = DateTime {
    yearMonthDay :: YearMonthDay
  , dayName      :: Maybe Text
  , hourMinute   :: Maybe (Hour,Minute)
  , repeater     :: Maybe Repeater
  , delay        :: Maybe Delay
  } deriving (Show, Eq, Generic)

data TimeUnit = UnitYear
              | UnitWeek
              | UnitMonth
              | UnitDay
              | UnitHour
              deriving (Show, Eq, Generic)

data RepeaterType = RepeatCumulate | RepeatCatchUp | RepeatRestart
                  deriving (Show, Eq, Generic)

data Repeater = Repeater {
    repeaterType  :: RepeaterType
  , repeaterValue :: Int
  , repeaterUnit  :: TimeUnit
  } deriving (Show, Eq, Generic)

data DelayType = DelayAll | DelayFirst
               deriving (Show, Eq, Generic)

data Delay = Delay {
    delayType  :: DelayType
  , delayValue :: Int
  , delayUnit  :: TimeUnit
  } deriving (Show, Eq, Generic)
