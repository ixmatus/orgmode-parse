{-|
Module      :  Data.OrgMode.Parse.Types
Copyright   :  © 2014 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  experimental

Types and utility functions.
-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.OrgMode.Parse.Types
( Document        (..)
, DateTime        (..)
, Delay           (..)
, DelayType       (..)
, Duration
, Headline        (..)
, Depth           (..)
, Depth      (..)
, PlanningKeyword (..)
, Plannings       (..)
, Priority        (..)
, Properties
, Repeater        (..)
, RepeaterType    (..)
, Section         (..)
, StateKeyword    (..)
, Stats           (..)
, Tag
, TimeUnit        (..)
, Timestamp       (..)
, TitleMeta       (..)
, YearMonthDay    (..)
, YearMonthDay'   (..)
) where

import           Control.Monad        (mzero)
import           Data.Aeson           ((.:), (.=))
import qualified Data.Aeson           as Aeson
import           Data.Hashable        (Hashable (..))
import           Data.HashMap.Strict  (HashMap, fromList, keys, toList)
import           Data.Text            (Text, pack)
import           Data.Thyme.Calendar  (YearMonthDay (..))
import           Data.Thyme.LocalTime (Hour, Minute)
import           GHC.Generics

data Document = Document {
    documentText      :: Text      -- ^ Text occurring before any Org headlines
  , documentHeadlines :: [Headline] -- ^ Toplevel Org headlines
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON Document where
instance Aeson.FromJSON Document where

newtype Depth = Depth Int
  deriving (Eq, Show, Num, Generic)

data TitleMeta = TitleMeta Text (Maybe Stats) (Maybe [Tag])
  deriving (Eq, Show)

data Headline = Headline
    { depth        :: Depth              -- ^ Org headline nesting depth (1 is at the top), e.g: * or ** or ***
    , stateKeyword :: Maybe StateKeyword -- ^ State of the headline, e.g: TODO, DONE
    , priority     :: Maybe Priority     -- ^ Headline priority, e.g: [#A]
    , title        :: Text               -- ^ Primary text of the headline
    , stats        :: Maybe Stats        -- ^ Fraction of subtasks completed, e.g: [33%] or [1/2]
    , tags         :: [Tag]              -- ^ Tags on the headline
    , section      :: Section            -- ^ The body underneath a headline
    , subHeadlines :: [Headline]          -- ^ A list of sub-headlines
    } deriving (Show, Eq, Generic)

type Properties = HashMap Text Text
type Clock      = (Maybe Timestamp, Maybe Duration)

data Section = Section {
      sectionPlannings  :: Plannings
    , sectionClocks     :: [Clock]
    , sectionProperties :: Properties
    , sectionParagraph  :: Text
  } deriving (Show, Eq, Generic)


data Timestamp = Timestamp {
    tsTime    :: DateTime
  , tsActive  :: Bool
  , tsEndTime :: Maybe DateTime
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON Timestamp where
instance Aeson.FromJSON Timestamp where


newtype YearMonthDay' = YMD' YearMonthDay
                        deriving (Show, Eq, Generic)

instance Aeson.ToJSON YearMonthDay' where
  toJSON (YMD' (YearMonthDay y m d)) =
    Aeson.object ["ymdYear"  .= y
             ,"ymdMonth" .= m
             ,"ymdDay"   .= d]

instance Aeson.FromJSON YearMonthDay' where
  parseJSON (Aeson.Object v) = do
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

instance Aeson.ToJSON DateTime where
instance Aeson.FromJSON DateTime where

data RepeaterType = RepeatCumulate | RepeatCatchUp | RepeatRestart
                  deriving (Show, Eq, Generic)

instance Aeson.ToJSON RepeaterType
instance Aeson.FromJSON RepeaterType

data Repeater = Repeater {
    repeaterType  :: RepeaterType
  , repeaterValue :: Int
  , repeaterUnit  :: TimeUnit
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON Repeater where
instance Aeson.FromJSON Repeater where

data DelayType = DelayAll | DelayFirst
               deriving (Show, Eq, Generic)

instance Aeson.ToJSON   DelayType where
instance Aeson.FromJSON DelayType where

data Delay = Delay {
    delayType  :: DelayType
  , delayValue :: Int
  , delayUnit  :: TimeUnit
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON Delay where
instance Aeson.FromJSON Delay where

data TimeUnit = UnitYear
              | UnitWeek
              | UnitMonth
              | UnitDay
              | UnitHour
              deriving (Show, Eq, Generic)

instance Aeson.ToJSON TimeUnit where
instance Aeson.FromJSON TimeUnit where

---------------------------------------------------------------------------
--instance Aeson.ToJSON Document where
--instance Aeson.FromJSON Document where

instance Aeson.ToJSON Depth where
instance Aeson.FromJSON Depth where

newtype StateKeyword = StateKeyword {unStateKeyword :: Text}
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON StateKeyword where
instance Aeson.FromJSON StateKeyword where


data PlanningKeyword = SCHEDULED | DEADLINE | CLOSED
  deriving (Show, Eq, Enum, Ord, Generic)

instance Aeson.ToJSON PlanningKeyword where
instance Aeson.FromJSON PlanningKeyword where

--instance (Aeson.ToJSON k, Aeson.ToJSON v) => Aeson.ToJSON (HashMap k v) where
--  toJSON hm = Aeson.object hm

newtype Plannings = Plns (HashMap PlanningKeyword Timestamp)
                  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Plannings where
  toJSON (Plns hm) = Aeson.object $ map jPair (toList hm)
    where jPair (k, v) = pack (show k) .= Aeson.toJSON v

instance Aeson.FromJSON Plannings where
  parseJSON (Aeson.Object v) = Plns . fromList <$> (traverse jPair (keys v))
    where jPair k = v .: k
  parseJSON _ = mzero

instance Aeson.ToJSON Section where
instance Aeson.FromJSON Section where

instance Aeson.ToJSON Headline where
instance Aeson.FromJSON Headline where

data Priority = A | B | C
  deriving (Show, Read, Eq, Ord, Generic)

instance Aeson.ToJSON Priority where
instance Aeson.FromJSON Priority where
type Tag = Text

data Stats = StatsPct Int
           | StatsOf  Int Int
           deriving (Show, Eq, Generic)

instance Aeson.ToJSON Stats where
instance Aeson.FromJSON Stats where

type Duration = (Hour,Minute)

instance Hashable PlanningKeyword where
  hashWithSalt salt k = hashWithSalt salt (fromEnum k)

-- -- This might be the form to use if we were supporting <diary> timestamps
-- data Timestamp = Dairy Text
--                | Time  TimestampTime
--               deriving (Show, Eq, Generic)



