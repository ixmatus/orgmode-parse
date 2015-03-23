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

module Data.OrgMode.Parse.Types
( Heading  (..)
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
) where

import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)
import           Data.Thyme.LocalTime (LocalTime (..))
import           Data.Tree            (Forest(..),Tree(..))

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
    , state    :: Maybe TodoState
    , title    :: Text
    , keywords :: [Keyword]
    } deriving (Show, Eq)


data Priority = A | B | C | Unknown
  deriving (Show, Read, Eq, Ord)

newtype TodoState = TodoState Text
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
  deriving (Show, Eq)

data Timestamp = Active LocalTime | Inactive LocalTime
  deriving (Show, Eq)

newtype Open = Open Char
newtype Close = Close Char

data ClockEntry = ClockOngoing LocalTime
                | ClockInterval (LocalTime,LocalTime)
                deriving (Show, Eq)

instance A.ToJSON OrgSection where
  toJSON (OrgSection secHead secProps secSched secClocks) =
    A.Object ["heading"    .= A.toJSON secHead
             ,"properties" .= A.toJSON segProps
             ,"schedules"  .= A.toJSON secSched
             ,"clocks"     .= A.toJSON segClocks
             ]

instance A.ToJSON Heading where
  toJSON (Heading hLevel hPr hTodoState hTitle hTags) =
    A.Object ["level"     .= A.Number hLevel
             ,"priority"  .= A.toJSON hPr
             ,"todoState" .= A.toJSON hTodoState
             ,"title"     .= A.Text hTitle
             ,"tags"      .= A.toJSON hTags
             ]

instance A.ToJSON Schedule where
  toJSON (Schedule sType sTs sRecur) =
    A.Object ["type"  .= show sType
             ,"time"    .= A.toJSON sTs
             ,"recur" .= A.toJSON sRecur
             ]

instance A.ToJSON Timestamp where
  toJSON (Active t)   = A.Object
                        ["timestamp"       .= show t
                        ,"timestampActive" .= True]
  toJSON (Inactive t) = A.Object
                        ["timestamp"       .= show t
                        ,"timestampActive" .= False]

instance A.ToJSON v => A.ToJSON PropertDrawer Text v where
  toJSON (PropertyDrawer m) = A.toJSON m

instance A.ToJSON Priority where
  toJSON A = A.String "A"
  toJSON B = A.String "B"
  toJSON C = A.String "C"
  toJSON _ = A.Null

instance A.ToJSON ClockEntry where
  toJSON (ClockOngoing t) =
      A.Array [A.toJSON t]
    toJSON (ClockInterval (t1,t2)) =
      A.Array (map A.toJSON [t1,t2])
