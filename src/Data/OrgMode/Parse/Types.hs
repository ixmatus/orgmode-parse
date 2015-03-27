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
, Plannings (..)
, TodoKeyword  (..)
, Duration
, PlanningKeyword (..)
, Properties
, Timestamp    (..)
, DateTime (..)
, Stats (..)
, Tag
, TimeUnit (..)
, RepeaterType (..)
, Repeater (..)
, DelayType (..)
, Delay (..)
, YearMonthDay(..)
, YearMonthDay'(..)
) where

import           Control.Applicative
import           Control.Monad        (mzero)
import qualified Data.Aeson           as A
import           Data.Aeson           ((.=),(.:))
import           Data.Hashable        (Hashable(..))
import           Data.HashMap.Strict  (HashMap, fromList, toList, keys)
import           Data.Text            (Text, pack)
import           Data.Thyme.Calendar  (YearMonthDay(..))
import           Data.Thyme.LocalTime (Hour, Minute)
import           Data.Traversable
import           GHC.Generics


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

type Properties = HashMap Text Text
type Clock      = (Maybe Timestamp, Maybe Duration)

data Section = Section {
      sectionPlannings  :: Plannings
    , sectionProperties :: Properties
    , sectionClocks     :: [Clock]
    , sectionParagraph  :: Text
  } deriving (Show, Eq, Generic)


data Timestamp = Timestamp {
    tsTime    :: DateTime
  , tsActive  :: Bool
  , tsEndTime :: Maybe DateTime
  } deriving (Show, Eq, Generic)

instance A.ToJSON Timestamp where
instance A.FromJSON Timestamp where


newtype YearMonthDay' = YMD' YearMonthDay
                        deriving (Show, Eq, Generic)

instance A.ToJSON YearMonthDay' where
  toJSON (YMD' (YearMonthDay y m d)) =
    A.object ["ymdYear"  .= y
             ,"ymdMonth" .= m
             ,"ymdDay"   .= d]

instance A.FromJSON YearMonthDay' where
  parseJSON (A.Object v) = do
    y <- v .: "ymdYear"
    m <- v .: "ymdMonth"
    d <- v .: "ymdDay"
    return (YMD' (YearMonthDay y m d))
  parseJSON _ = mzero

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

---------------------------------------------------------------------------
data Document = Document {
    openingText :: Text
  , topHeadings :: [Heading]
  } deriving (Show, Eq, Generic)

--instance A.ToJSON Document where
--instance A.FromJSON Document where

data TodoKeyword = TODO
                 | DONE
                 | OtherKeyword Text
  deriving (Show, Eq, Generic)

instance A.ToJSON TodoKeyword where
instance A.FromJSON TodoKeyword where


data PlanningKeyword = SCHEDULED | DEADLINE | CLOSED
  deriving (Show, Eq, Enum, Ord, Generic)

instance A.ToJSON PlanningKeyword where
instance A.FromJSON PlanningKeyword where

--instance (A.ToJSON k, A.ToJSON v) => A.ToJSON (HashMap k v) where
--  toJSON hm = A.object hm

newtype Plannings = Plns (HashMap PlanningKeyword Timestamp)
                  deriving (Show, Eq, Generic)

instance A.ToJSON Plannings where
  toJSON (Plns hm) = A.object $ map jPair (toList hm)
    where jPair (k, v) = pack (show k) .= A.toJSON v

instance A.FromJSON Plannings where
  parseJSON (A.Object v) = Plns . fromList <$> (traverse jPair (keys v))
    where jPair k = v .: k
  parseJSON _ = mzero

instance A.ToJSON Section where
instance A.FromJSON Section where

instance A.ToJSON Heading where
instance A.FromJSON Heading where

data Priority = A | B | C
  deriving (Show, Read, Eq, Ord, Generic)

instance A.ToJSON Priority where
instance A.FromJSON Priority where
type Tag = Text

data Stats = StatsPct Int
           | StatsOf  Int Int
           deriving (Show, Eq, Generic)

instance A.ToJSON Stats where
instance A.FromJSON Stats where

type Duration = (Hour,Minute)

instance Hashable PlanningKeyword where
  hashWithSalt salt k = hashWithSalt salt (fromEnum k)

-- -- This might be the form to use if we were supporting <diary> timestamps
-- data Timestamp = Dairy Text
--                | Time  TimestampTime
--               deriving (Show, Eq, Generic)



