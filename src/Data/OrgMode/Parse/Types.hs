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
( OrgDocument (..)
, OrgSection (..)
, Heading  (..)
, Priority (..)
, State    (..)
, Keyword  (..)
, PropertyDrawer (..)
, Schedule     (..)
, ScheduleType (..)
, Timestamp    (..)
, Open         (..)
, Close        (..)
, toPriority
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
data OrgDocument = OrgDocument {
  headings :: Forest OrgSection
  } deriving (Show, Eq)

data OrgSection = OrgSection {
    sectionHeading :: Heading
  , sectionPropertyDrawer :: Maybe (PropertyDrawer Text Text)
  , sectionSchedules :: [Schedule]
  , sectionClockEntries :: [ClockEntry]
  } deriving (Show, Eq)

data Heading = Heading
    { level    :: Int
    , priority :: Maybe Priority
    , state    :: Maybe State
    , title    :: Text
    , keywords :: [Keyword]
    } deriving (Show, Eq)


data Priority = A | B | C | Unknown
  deriving (Show, Read, Eq, Ord)

newtype State = State Text
  deriving (Show, Eq)

newtype Keyword = Keyword Text
  deriving (Show, Eq, Ord)

toPriority :: Text -> Priority
toPriority "A" = A
toPriority "B" = B
toPriority "C" = C
toPriority _   = Unknown

----------------------------------------------------------------------------

newtype PropertyDrawer k v = PropertyDrawer (HashMap k v)
  deriving (Show, Eq)

----------------------------------------------------------------------------
data Schedule = Schedule
    { schedule_type :: ScheduleType
    , timestamp     :: Maybe Timestamp
    , recurring     :: Maybe Text
    } deriving (Show, Eq)

data ScheduleType = SCHEDULED | DEADLINE | APPOINTMENT
  deriving (Show, Eq, Generic)

data Timestamp = Dairy Text
               | Time  TimestampTime
               deriving (Show, Eq, Generic)

data TimestampTime = TimestampTime {
    tsTimes  :: Either DateTime (DateTime, DateTime)
  , tsActive :: Bool
  } deriving (Eq, Show)

data DateTime = DateTime {
    yearMonthDay :: YearMonthDay
  , dayName      :: Maybe Text
  , hourMinute   :: (Hour,Minute)
  , repeater     :: Maybe Repeater
  , delay        :: Maybe Delay
  } deriving (Eq, Show)

data TimeUnit = UnitYear
              | UnitWeek
              | UnitMonth
              | UnitDay
              | UnitHour
              deriving (Eq, Show, Generic)

data RepeaterType = RepeatCumulate | RepeatCatchUp | RepeatRestart
                  deriving (Eq, Show, Generic)

data Repeater = Repeater {
    repeaterType :: RepeaterType
  , repeaterValue :: (Int, TimeUnit)
  } deriving (Eq, Show, Generic)

data DelayType = DelayAll | DelayFirst
               deriving (Eq, Show, Generic)

data Delay = Delay {
    delayType :: DelayType
  , delayValue :: (Int, TimeUnit)
  } deriving (Eq, Show, Generic)


newtype Open = Open Char
newtype Close = Close Char

data ClockEntry = ClockOngoing LocalTime
                | ClockInterval (LocalTime,LocalTime)
                deriving (Show, Eq)
