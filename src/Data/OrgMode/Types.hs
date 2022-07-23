{-|
Module      :  Data.OrgMode.Types
Copyright   :  © 2014 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  experimental

Types for the AST of an org-mode document.
-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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
, TimePart          (..)
, TimeUnit          (..)
, Timestamp         (..)
, YearMonthDay      (..)
, Content           (..)
, MarkupText        (..)
, Item              (..)
, sectionDrawer
) where

import           Control.Monad              (mzero)
import           Data.Aeson                 (FromJSON (..), ToJSON (..),
                                             Value (..), defaultOptions,
                                             genericToEncoding, object, (.:),
                                             (.=))
import           Data.Data                  (Data(..), Typeable)
import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import           Data.Semigroup             (Semigroup)
import           Data.Text                  (Text)
import           Data.Thyme.Calendar        (YearMonthDay (..))
import           Data.Thyme.LocalTime       (Hour, Hours, Minute, Minutes)
import           GHC.Generics
import           GHC.Natural                (Natural)

#if MIN_VERSION_base(4,11,0)
instance Semigroup Natural where
  a <> b = a + b
#endif

instance Monoid Natural where
#if ! MIN_VERSION_base(4,11,0)
  a `mappend` b = a + b
#endif
  mempty = 0

-- | Org-mode document.
data Document = Document
  { documentText      :: Text       -- ^ Text occurring before any Org headlines
  , documentHeadlines :: [Headline] -- ^ Toplevel Org headlines
  } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Document where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Document

-- | Headline within an org-mode document.
data Headline = Headline
  { depth        :: Depth              -- ^ Org headline nesting depth (1 is at the top), e.g: * or ** or ***
  , stateKeyword :: Maybe StateKeyword -- ^ State of the headline, e.g: TODO, DONE
  , priority     :: Maybe Priority     -- ^ Headline priority, e.g: [#A]
  , title        :: Text               -- ^ Primary text of the headline
  , timestamp    :: Maybe Timestamp    -- ^ A timestamp that may be embedded in the headline
  , stats        :: Maybe Stats        -- ^ Fraction of subtasks completed, e.g: [33%] or [1/2]
  , tags         :: [Text]             -- ^ Tags on the headline
  , section      :: Section            -- ^ The body underneath a headline
  , subHeadlines :: [Headline]         -- ^ A list of sub-headlines
  } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Headline where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Headline

-- | Headline nesting depth.
newtype Depth = Depth Natural
  deriving (Eq, Show, Num, ToJSON, FromJSON, Generic, Typeable, Data)

-- | Section of text directly following a headline.
data Section = Section
  { sectionTimestamp  :: Maybe Timestamp -- ^ A headline's section timestamp
  , sectionPlannings  :: [Planning]      -- ^ A list of planning records
  , sectionClocks     :: [Clock]         -- ^ A list of clocks
  , sectionProperties :: Properties      -- ^ A map of properties from the :PROPERTY: drawer
  , sectionLogbook    :: Logbook         -- ^ A list of clocks from the :LOGBOOK: drawer
  , sectionContents   :: [Content]       -- ^ Content of Section
  } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Section where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Section

sectionDrawer :: Section -> [Content]
sectionDrawer s = filter isDrawer (sectionContents s)
  where
  isDrawer (Drawer _ _) = True
  isDrawer _            = False

newtype Properties = Properties { unProperties :: InsOrdHashMap Text Text }
  deriving (Show, Eq, Semigroup, Monoid, ToJSON, FromJSON, Generic, Typeable, Data)

data MarkupText
  = Plain         Text
  | LaTeX         Text
  | Verbatim      Text
  | Code          Text
  | Bold          [MarkupText]
  | Italic        [MarkupText]
  | UnderLine     [MarkupText]
  | Strikethrough [MarkupText]
  | HyperLink
    { link        :: Text
    , description :: Maybe Text
    }
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON MarkupText where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON MarkupText

newtype Item = Item [Content]
  deriving (Show, Eq, Semigroup, Monoid, ToJSON, FromJSON, Generic, Typeable, Data)

data Content
  =
    OrderedList   [Item]
  | UnorderedList [Item]
  | Paragraph     [MarkupText]
  | Drawer
    { name     :: Text
    , contents :: Text
    } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Content where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Content

type Drawer = Content

newtype Logbook = Logbook { unLogbook :: [Clock] }
  deriving (Show, Eq, Semigroup, Monoid, ToJSON, FromJSON, Generic, Typeable, Data)

-- | Sum type indicating the active state of a timestamp.
data ActiveState
  = Active
  | Inactive
  deriving (Show, Eq, Read, Generic, Typeable, Data)

instance ToJSON ActiveState where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ActiveState

newtype Clock = Clock { unClock :: (Maybe Timestamp, Maybe Duration) }
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Typeable, Data)

-- | A generic data type for parsed org-mode time stamps, e.g:
--
-- > <2015-03-27 Fri 10:20>
-- > [2015-03-27 Fri 10:20 +4h]
-- > <2015-03-27 Fri 10:20>--<2015-03-28 Sat 10:20>
data Timestamp = Timestamp
  { tsTime    :: DateTime       -- ^ A datetime stamp
  , tsActive  :: ActiveState    -- ^ Active or inactive?
  , tsEndTime :: Maybe DateTime -- ^ A end-of-range datetime stamp
  } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Timestamp where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Timestamp

instance ToJSON YearMonthDay where
  toJSON (YearMonthDay y m d) =
    object
      [ "year"  .= y
      , "month" .= m
      , "day"   .= d
      ]

instance FromJSON YearMonthDay where
  parseJSON (Object v) = do
    y <- v .: "year"
    m <- v .: "month"
    d <- v .: "day"
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
  deriving (Eq, Ord, Show, Generic, Typeable, Data)

instance ToJSON TimePart where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TimePart

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
    } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON DateTime where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DateTime

-- | A sum type representing the repeater type of a repeater interval
-- in a org-mode timestamp.
data RepeaterType
  = RepeatCumulate
  | RepeatCatchUp
  | RepeatRestart
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON RepeaterType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RepeaterType

-- | A data type representing a repeater interval in a org-mode
-- timestamp.
data Repeater = Repeater
  { repeaterType  :: RepeaterType -- ^ Type of repeater
  , repeaterValue :: Natural      -- ^ Repeat value
  , repeaterUnit  :: TimeUnit     -- ^ Repeat time unit
  } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Repeater where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Repeater

-- | A sum type representing the delay type of a delay value.
data DelayType
  = DelayAll
  | DelayFirst
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON DelayType where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DelayType

-- | A data type representing a delay value.
data Delay = Delay
  { delayType  :: DelayType -- ^ Type of delay
  , delayValue :: Natural   -- ^ Delay value
  , delayUnit  :: TimeUnit  -- ^ Delay time unit
  } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Delay where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Delay

-- | A sum type representing the time units of a delay.
data TimeUnit
  = UnitYear
  | UnitWeek
  | UnitMonth
  | UnitDay
  | UnitHour
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON TimeUnit where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TimeUnit

-- | A type representing a headline state keyword, e.g: @TODO@,
-- @DONE@, @WAITING@, etc.
newtype StateKeyword = StateKeyword { unStateKeyword :: Text }
  deriving (Show, Eq, Semigroup, Monoid, ToJSON, FromJSON, Generic, Typeable, Data)

-- | A sum type representing the planning keywords.
data PlanningKeyword = SCHEDULED | DEADLINE | CLOSED
  deriving (Show, Eq, Enum, Ord, Generic, Typeable, Data)

instance ToJSON PlanningKeyword where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PlanningKeyword

-- | A type representing a map of planning timestamps.
data Planning = Planning
  { keyword   :: PlanningKeyword
  , timestamp :: Timestamp
  } deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Planning where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Planning

-- | A sum type representing the three default priorities: @A@, @B@,
-- and @C@.
data Priority = A | B | C
  deriving (Show, Read, Eq, Ord, Generic, Typeable, Data)

instance ToJSON Priority where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Priority

-- | A data type representing a stats value in a headline, e.g @[2/3]@
-- in this headline:
--
-- > * TODO [2/3] work on orgmode-parse
data Stats
  = StatsPct Natural
  | StatsOf  Natural Natural
  deriving (Show, Eq, Generic, Typeable, Data)

instance ToJSON Stats where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Stats

type Duration = (Hour,Minute)
