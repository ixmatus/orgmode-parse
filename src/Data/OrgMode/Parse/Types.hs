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
, PlanningKeyword (..)
, Timestamp    (..)
, DateTime (..)
, Stats (..)
, Tag
, TimeUnit (..)
, RepeaterType (..)
, Repeater (..)
, DelayType (..)
, Delay (..)
) where

import qualified Data.Aeson           as A
import           Data.Hashable        (Hashable(..))
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)
import           Data.Thyme.Calendar  (YearMonthDay(..))
import           Data.Thyme.LocalTime (Hour, Minute)
import           GHC.Generics

----------------------------------------------------------------------------
data Document = Document {
    openingText :: Text
  , topHeadings :: [Heading]
  } deriving (Show, Eq, Generic)

instance A.ToJSON Document where
instance A.FromJSON Document where

data Section = Section {
      sectionPlannings  :: HashMap PlanningKeyword Timestamp
    , sectionProperties :: HashMap Text         Text
    , sectionClocks     :: [(Maybe Timestamp, Maybe Duration)]
    , sectionParagraph  :: Text
  } deriving (Show, Eq, Generic)

instance A.ToJSON Section where
instance A.FromJSON Section where

data Heading = Heading
    { level       :: Int                --
    , keyword     :: Maybe TodoKeyword  --
    , priority    :: Maybe Priority     -- Header line
    , title       :: Text               -- properties
    , stats       :: Maybe Stats        --
    , tags        :: [Tag]             --
    , section     :: Section            -- Next-line
    , subHeadings :: [Heading]          -- elements
    } deriving (Show, Eq, Generic)

instance A.ToJSON Heading where
instance A.FromJSON Heading where

data Priority = A | B | C
  deriving (Show, Read, Eq, Ord, Generic)

instance A.ToJSON Priority where
instance A.FromJSON Priority where

data TodoKeyword = TODO
                 | DONE
                 | OtherKeyword Text
  deriving (Show, Eq, Generic)

instance A.ToJSON TodoKeyword where
instance A.FromJSON TodoKeyword where

type Tag = Text

data Stats = StatsPct Int
           | StatsOf  Int Int
           deriving (Show, Eq, Generic)

instance A.ToJSON Stats where
instance A.FromJSON Stats where

type Duration = (Hour,Minute)

data PlanningKeyword = SCHEDULED | DEADLINE | CLOSED
  deriving (Show, Eq, Enum, Ord, Generic)

instance A.ToJSON PlanningKeyword where
instance A.FromJSON PlanningKeyword where

instance Hashable PlanningKeyword where
  hashWithSalt salt k = hashWithSalt salt (fromEnum k)

-- -- This might be the form to use if we were supporting <diary> timestamps
-- data Timestamp = Dairy Text
--                | Time  TimestampTime
--               deriving (Show, Eq, Generic)

data Timestamp = Timestamp {
    tsTime    :: DateTime
  , tsActive  :: Bool
  , tsEndTime :: Maybe DateTime
  } deriving (Show, Eq, Generic)

instance A.ToJSON Timestamp where
instance A.FromJSON Timestamp where


data DateTime = DateTime {
    yearMonthDay :: YearMonthDay'
  , dayName      :: Maybe Text
  , hourMinute   :: Maybe (Hour,Minute)
  , repeater     :: Maybe Repeater
  , delay        :: Maybe Delay
  } deriving (Show, Eq, Generic)

instance A.ToJSON DateTime where
instance A.FromJSON DateTime where

data RepeaterType = RepeatCumulate | RepeatCatchUp | RepeatRestart
                  deriving (Show, Eq, Generic)

instance A.ToJSON RepeaterType
instance A.FromJSON RepeaterType

data Repeater = Repeater {
    repeaterType  :: RepeaterType
  , repeaterValue :: Int
  , repeaterUnit  :: TimeUnit
  } deriving (Show, Eq, Generic)

instance A.ToJSON Repeater where
instance A.FromJSON Repeater where

data DelayType = DelayAll | DelayFirst
               deriving (Show, Eq, Generic)

instance A.ToJSON   DelayType where
instance A.FromJSON DelayType where

data Delay = Delay {
    delayType  :: DelayType
  , delayValue :: Int
  , delayUnit  :: TimeUnit
  } deriving (Show, Eq, Generic)

instance A.ToJSON Delay where
instance A.FromJSON Delay where

data TimeUnit = UnitYear
              | UnitWeek
              | UnitMonth
              | UnitDay
              | UnitHour
              deriving (Show, Eq, Generic)

instance A.ToJSON TimeUnit where
instance A.FromJSON TimeUnit where


newtype YearMonthDay' = YMD' YearMonthDay

instance A.ToJSON YearMonthDay' where
  toJSON (YMD' (YearMonthDay y m d)) =
    A.Object ["ymdYear"  .= y
             ,"ymdMonth" .= m
             ,"ymdDay"   .= d]
  fromJSON (Object v) = (YearMonthDay' . YearMonthDay)
                        <$> v .: "ymdYear"
                        <*> v .: "ymdMonth"
                        <*> v .: "ymdDay"
