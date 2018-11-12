{-|
Module      :  Data.OrgMode.Types
Copyright   :  © 2014 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  experimental

Types for the AST of an org-mode document.
-}

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DataKinds                  #-}

{-# OPTIONS -fno-warn-orphans           #-}

module Data.OrgMode.Types
( ActiveState       (..)
, BracketedDateTime (..)
, Clock             (..)
, DateTime          (..)
, Delay             (..)
, DelayType         (..)
, Depth             (..)
, Document          (..)
, Drawer
, Duration
, Headline          (..)
, Logbook           (..)
, PlanningKeyword   (..)
, Planning          (..)
, Priority          (..)
, Properties        (..)
, Repeater          (..)
, RepeaterType      (..)
, Section           (..)
, StateKeyword      (..)
, Stats             (..)
, Tag
, TimePart          (..)
, TimeUnit          (..)
, Timestamp         (..)
, YearMonthDay      (..)
, Block             (..)
, MarkupText        (..)
, Item              (..)
, sectionDrawer
) where

import           Control.Monad                     (mzero)
import           Data.Aeson                        ((.:), (.=), FromJSON, ToJSON)
import           Data.HashMap.Strict.InsOrd        (InsOrdHashMap)
import           Data.Hashable                     (Hashable (..))
import           Data.Semigroup                    (Semigroup)
import           Data.Text                         (Text)
import           Data.Thyme.Calendar               (YearMonthDay (..))
import           Data.Thyme.LocalTime              (Hour, Hours, Minute, Minutes)
import           GHC.Generics
import           GHC.Natural                       (Natural)
import           Dhall

--import qualified Dhall.TypeCheck
--import qualified Dhall.Parser
import qualified Dhall.Core
import qualified Data.Aeson                        as Aeson
import qualified Data.HashMap.Strict.InsOrd        as InsOrd

instance Semigroup Natural where
  a <> b = a + b

instance Monoid Natural where
  mempty = 0

deriving instance Inject YearMonthDay

instance Inject (InsOrdHashMap Text Text) where
  injectWith _ = InputType{..}
    where
      embed m =
        Dhall.Core.RecordLit (fmap (Dhall.Core.TextLit . Dhall.Core.Chunks []) m)

      declared =
        Dhall.Core.Record InsOrd.empty

-- | Org-mode document.
data Document = Document
  { documentText      :: Text       -- ^ Text occurring before any Org headlines
  , documentHeadlines :: [Headline] -- ^ Toplevel Org headlines
  } deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

-- | Headline within an org-mode document.
data Headline = Headline
  { depth        :: Depth              -- ^ Org headline nesting depth (1 is at the top), e.g: * or ** or ***
  , stateKeyword :: Maybe StateKeyword -- ^ State of the headline, e.g: TODO, DONE
  , priority     :: Maybe Priority     -- ^ Headline priority, e.g: [#A]
  , title        :: Text               -- ^ Primary text of the headline
  , timestamp    :: Maybe Timestamp    -- ^ A timestamp that may be embedded in the headline
  , stats        :: Maybe Stats        -- ^ Fraction of subtasks completed, e.g: [33%] or [1/2]
  , tags         :: [Tag]              -- ^ Tags on the headline
  , section      :: Section            -- ^ The body underneath a headline
  , subHeadlines :: [Headline]         -- ^ A list of sub-headlines
  } deriving (Show, Eq, ToJSON, FromJSON, Inject,  Generic)

-- | Headline nesting depth.
newtype Depth = Depth Natural
  deriving (Eq, Show, Generic)
  deriving newtype Num
  deriving anyclass ToJSON
  deriving anyclass FromJSON
  deriving anyclass Inject

-- | Section of text directly following a headline.
data Section = Section
  { sectionTimestamp  :: Maybe Timestamp -- ^ A headline's section timestamp
  , sectionPlannings  :: [Planning]      -- ^ A list of planning records
  , sectionClocks     :: [Clock]         -- ^ A list of clocks
  , sectionProperties :: Properties      -- ^ A map of properties from the :PROPERTY: drawer
  , sectionLogbook    :: Logbook         -- ^ A list of clocks from the :LOGBOOK: drawer
  , sectionBlocks     :: [Block]  -- ^ Content of Section
  } deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

sectionDrawer :: Section -> [Block]
sectionDrawer s = filter isDrawer (sectionBlocks s)
  where
  isDrawer (Drawer _ _) = True
  isDrawer _ = False

newtype Properties = Properties { unProperties :: InsOrdHashMap Text Text }
  deriving (Show, Eq, Generic)
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving anyclass ToJSON
  deriving anyclass FromJSON
  deriving anyclass Inject

data MarkupText
  = Plain         Text
  | LaTeX         Text
  | Verbatim      Text
  | Code          Text
  | Bold          [MarkupText]
  | Italic        [MarkupText]
  | UnderLine     [MarkupText]
  | Strikethrough [MarkupText]
  deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

newtype Item = Item [Block]
  deriving (Show, Eq, Generic)
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving anyclass ToJSON
  deriving anyclass FromJSON
  deriving anyclass Inject

data Block
  =
    OrderedList   [Item]
  | UnorderedList [Item]
  | Paragraph     [MarkupText]
  | Drawer
    { name     :: Text
    , contents :: Text
    } deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

type Drawer = Block

newtype Logbook = Logbook { unLogbook :: [Clock] }
  deriving (Show, Eq, Generic)
  deriving newtype Semigroup
  deriving newtype Monoid
  deriving anyclass ToJSON
  deriving anyclass FromJSON
  deriving anyclass Inject

-- | Sum type indicating the active state of a timestamp.
data ActiveState
  = Active
  | Inactive
  deriving (Show, Eq, Read, ToJSON, FromJSON, Inject, Generic)

newtype Clock = Clock { unClock :: (Maybe Timestamp, Maybe Duration) }
  deriving (Show, Eq, Generic)
  deriving anyclass ToJSON
  deriving anyclass FromJSON
  deriving anyclass Inject

-- | A generic data type for parsed org-mode time stamps, e.g:
--
-- > <2015-03-27 Fri 10:20>
-- > [2015-03-27 Fri 10:20 +4h]
-- > <2015-03-27 Fri 10:20>--<2015-03-28 Sat 10:20>
data Timestamp = Timestamp
  { tsTime    :: DateTime       -- ^ A datetime stamp
  , tsActive  :: ActiveState    -- ^ Active or inactive?
  , tsEndTime :: Maybe DateTime -- ^ A end-of-range datetime stamp
  } deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

instance Aeson.ToJSON YearMonthDay where
  toJSON (YearMonthDay y m d) =
    Aeson.object
      [ "ymdYear"  .= y
      , "ymdMonth" .= m
      , "ymdDay"   .= d
      ]

instance Aeson.FromJSON YearMonthDay where
  parseJSON (Aeson.Object v) = do
    y <- v .: "ymdYear"
    m <- v .: "ymdMonth"
    d <- v .: "ymdDay"
    pure (YearMonthDay y m d)
  parseJSON _ = mzero

type Weekday = Text
type AbsTime = (Hours, Minutes)

-- | A data type for parsed org-mode bracketed datetime stamps, e.g:
--
-- > [2015-03-27 Fri 10:20 +4h]
data BracketedDateTime = BracketedDateTime
  { datePart    :: YearMonthDay
  , dayNamePart :: Maybe Weekday
  , timePart    :: Maybe TimePart
  , repeat      :: Maybe Repeater
  , delayPart   :: Maybe Delay
  , activeState :: ActiveState
  } deriving (Show, Eq)

-- | A sum type representing an absolute time part of a bracketed
-- org-mode datetime stamp or a time range between two absolute
-- timestamps.
data TimePart
  = AbsoluteTime   AbsTime
  | TimeStampRange (AbsTime, AbsTime)
  deriving (Eq, Ord, Show, ToJSON, FromJSON,  Generic)

-- | A data type for parsed org-mode datetime stamps.
--
-- TODO: why do we have this data type and BracketedDateTime? They
-- look almost exactly the same...
data DateTime
  = DateTime
    { yearMonthDay :: YearMonthDay
    , dayName      :: Maybe Text
    , hourMinute   :: Maybe (Hour,Minute)
    , repeater     :: Maybe Repeater
    , delay        :: Maybe Delay
    } deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

-- | A sum type representing the repeater type of a repeater interval
-- in a org-mode timestamp.
data RepeaterType
  = RepeatCumulate
  | RepeatCatchUp
  | RepeatRestart
  deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

-- | A data type representing a repeater interval in a org-mode
-- timestamp.
data Repeater = Repeater
  { repeaterType  :: RepeaterType -- ^ Type of repeater
  , repeaterValue :: Natural      -- ^ Repeat value
  , repeaterUnit  :: TimeUnit     -- ^ Repeat time unit
  } deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

-- | A sum type representing the delay type of a delay value.
data DelayType
  = DelayAll
  | DelayFirst
  deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

-- | A data type representing a delay value.
data Delay = Delay
  { delayType  :: DelayType -- ^ Type of delay
  , delayValue :: Natural   -- ^ Delay value
  , delayUnit  :: TimeUnit  -- ^ Delay time unit
  } deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

-- | A sum type representing the time units of a delay.
data TimeUnit
  = UnitYear
  | UnitWeek
  | UnitMonth
  | UnitDay
  | UnitHour
  deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

-- | A type representing a headline state keyword, e.g: @TODO@,
-- @DONE@, @WAITING@, etc.
newtype StateKeyword = StateKeyword { unStateKeyword :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype  Semigroup
  deriving newtype  Monoid
  deriving anyclass ToJSON
  deriving anyclass FromJSON
  deriving anyclass Inject

-- | A sum type representing the planning keywords.
data PlanningKeyword = SCHEDULED | DEADLINE | CLOSED
  deriving (Show, Eq, Enum, Ord, Generic)
  deriving anyclass ToJSON
  deriving anyclass FromJSON
  deriving anyclass Inject

-- | A type representing a map of planning timestamps.
data Planning = Planning
  { keyword   :: PlanningKeyword
  , timestamp :: Timestamp
  } deriving (Show, Eq, Generic)
    deriving anyclass ToJSON
    deriving anyclass FromJSON
    deriving anyclass Inject

-- | A sum type representing the three default priorities: @A@, @B@,
-- and @C@.
data Priority = A | B | C
  deriving
    ( Show
    , Read
    , Eq
    , Ord
    , ToJSON
    , FromJSON
    , Inject
    , Generic
    )

type Tag = Text

-- | A data type representing a stats value in a headline, e.g @[2/3]@
-- in this headline:
--
-- > * TODO [2/3] work on orgmode-parse
data Stats
  = StatsPct Natural
  | StatsOf  Natural Natural
  deriving (Show, Eq, ToJSON, FromJSON, Inject, Generic)

type Duration = (Hour,Minute)

instance Hashable PlanningKeyword where
  hashWithSalt salt k = hashWithSalt salt (fromEnum k)

