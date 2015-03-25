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
      sectionHeading  :: Heading
    , sectionElements :: [Element]
  } deriving (Show, Eq)

data Heading = Heading
    { level    :: Int
    , keyword  :: Maybe TodoKeyword
    , priority :: Maybe Priority
    , title    :: Text
    , tags     :: [Text]
    , stats    :: Maybe Stats
    } deriving (Show, Eq)


data Priority = A | B | C
  deriving (Show, Read, Eq, Ord)

data TodoKeyword = TODO
                 | DONE
                 | OtherKeyword Text
  deriving (Show, Eq)

newtype Keyword = Keyword Text
  deriving (Show, Eq, Ord)

-- TODO Parse priority rather than pattern matching here
toPriority :: Text -> Priority
toPriority "A" = A
toPriority "B" = B
toPriority "C" = C

data Stats = StatsPtc Int
           | StatsOf  Int Int
           deriving (Eq, Show, Generic)

----------------------------------------------------------------------------
data Element = Clock    (Maybe Timestamp) (Maybe Duration)
             | Planning [(PlanningType, Maybe Timestamp)]
             | PropertyDrawer (HashMap Text Text)
             deriving (Eq, Show, Generic)

type Duration = (Hour,Minute)

data PlanningType = SCHEDULED | DEADLINE | APPOINTMENT
  deriving (Show, Eq, Generic)

-- -- This might be the form to use if we were supporting <diary> timestamps
-- data Timestamp = Dairy Text
--                | Time  TimestampTime
--               deriving (Show, Eq, Generic)

data Timestamp = Timestamp {
    tsTime    :: DateTime
  , tsActive  :: Bool
  , tsEndTime :: Maybe DateTime
  } deriving (Eq, Show)

data DateTime = DateTime {
    yearMonthDay :: YearMonthDay
  , dayName      :: Maybe Text
  , hourMinute   :: Maybe (Hour,Minute)
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
    repeaterType  :: RepeaterType
  , repeaterValue :: Int
  , repeaterUnit  :: TimeUnit
  } deriving (Eq, Show, Generic)

data DelayType = DelayAll | DelayFirst
               deriving (Eq, Show, Generic)

data Delay = Delay {
    delayType  :: DelayType
  , delayValue :: Int
  , delayUnit  :: TimeUnit
  } deriving (Eq, Show, Generic)



data ClockEntry = ClockOngoing LocalTime
                | ClockInterval (LocalTime,LocalTime)
                deriving (Show, Eq)

-- instance A.ToJSON OrgSection where
--   toJSON (OrgSection secHead secProps secSched secClocks) =
--     A.Object ["heading"    .= A.toJSON secHead
--              ,"properties" .= A.toJSON segProps
--              ,"schedules"  .= A.toJSON secSched
--              ,"clocks"     .= A.toJSON segClocks
--              ]

-- instance A.ToJSON Heading where
--   toJSON (Heading hLevel hPr hTodoState hTitle hTags) =
--     A.Object ["level"     .= A.Number hLevel
--              ,"priority"  .= A.toJSON hPr
--              ,"todoState" .= A.toJSON hTodoState
--              ,"title"     .= A.Text hTitle
--              ,"tags"      .= A.toJSON hTags
--              ]

-- instance A.ToJSON Schedule where
--   toJSON (Schedule sType sTs sRecur) =
--     A.Object ["type"  .= show sType
--              ,"time"    .= A.toJSON sTs
--              ,"recur" .= A.toJSON sRecur
--              ]

-- instance A.ToJSON Timestamp where
--   toJSON (Active t)   = A.Object
--                         ["timestamp"       .= show t
--                         ,"timestampActive" .= True]
--   toJSON (Inactive t) = A.Object
--                         ["timestamp"       .= show t
--                         ,"timestampActive" .= False]

-- instance A.ToJSON v => A.ToJSON PropertDrawer Text v where
--   toJSON (PropertyDrawer m) = A.toJSON m

-- instance A.ToJSON Priority where
--   toJSON A = A.String "A"
--   toJSON B = A.String "B"
--   toJSON C = A.String "C"
--   toJSON _ = A.Null

-- instance A.ToJSON ClockEntry where
--   toJSON (ClockOngoing t) =
--       A.Array [A.toJSON t]
--     toJSON (ClockInterval (t1,t2)) =
--       A.Array (map A.toJSON [t1,t2])
