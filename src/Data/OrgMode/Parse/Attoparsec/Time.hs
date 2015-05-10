-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Time
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode active and inactive timestamps.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.OrgMode.Parse.Attoparsec.Time where

import           Control.Applicative        (pure, some,
                                             (*>), (<$>), (<*), (<*>), (<|>))
import           Control.Monad              (liftM)
import qualified Data.Attoparsec.ByteString as AB
import           Data.Attoparsec.Text       as T
import           Data.Attoparsec.Types      as TP (Parser)
import           Data.Attoparsec.Combinator as TP
import qualified Data.ByteString.Char8      as BS
import           Data.HashMap.Strict        (HashMap, fromList)
import           Data.Maybe                 (listToMaybe)
import           Data.Text                  as Text (Text, pack, unpack,
                                                     unwords)
import           Data.Thyme.Format          (buildTime, timeParser)
import           Data.Thyme.LocalTime       (Hours, Minutes)
import           Prelude                    hiding (concat, null, takeWhile,
                                             unwords, words)
import           System.Locale              (defaultTimeLocale)

import           Data.OrgMode.Parse.Types

-- | Parse a planning line.
--
-- Plannings inhabit a heading section and are formatted as a keyword
-- and a timestamp. There can be more than one, but they are all on
-- the same line e.g:
--
-- > DEADLINE: <2015-05-10 17:00> CLOSED: <2015-04-1612:00>
parsePlannings :: TP.Parser Text (HashMap PlanningKeyword Timestamp)
parsePlannings = fromList <$> (many' (planning <* skipSpace))
  where planning :: TP.Parser Text (PlanningKeyword, Timestamp)
        planning =  (,) <$> pType <* char ':' <*> (skipSpace *> parseTimestamp)
        pType    = choice [string "SCHEDULED" *> pure SCHEDULED
                          ,string "DEADLINE"  *> pure DEADLINE
                          ,string "CLOSED"    *> pure CLOSED
                          ]

-- | Parse a clock line.
--
-- A heading's section contains one line per clock entry. Clocks may
-- have a timestamp, a duration, both, or neither e.g.:
--
-- > CLOCK: [2014-12-10 Fri 2:30]--[2014-12-10 Fri 10:30] => 08:00
parseClock :: TP.Parser Text (Maybe Timestamp, Maybe Duration)
parseClock = (,) <$> (skipSpace *> string "CLOCK: " *> ts) <*> dur
  where
    ts  = option Nothing (Just <$> parseTimestamp)
    dur = option Nothing (Just <$> (string " => "
                                    *> skipSpace *> parseHM))

-- | Parse a timestamp.
--
-- Timestamps may be timepoints or timeranges, and they indicate
-- whether they are active or closed by using angle or square brackets
-- respectively.
--
-- Time ranges are formatted by infixing two timepoints with a double
-- hyphen, @--@; or, by appending two @hh:mm@ timestamps together in a
-- single timepoint with one hyphen @-@.
--
-- Each timepoint includes an optional repeater flag and an optional
-- delay flag.
parseTimestamp :: TP.Parser Text Timestamp
parseTimestamp = do
  (ts1, tsb1, act) <- transformBracketedDateTime <$> parseBracketedDateTime

  -- Such ew, so gross, much clean, very needed
  blk2 <- liftM (maybe Nothing (Just . transformBracketedDateTime))
                optionalBracketedDateTime

  -- TODO: this is grody, I'd like to refactor this truth table logic
  -- and make the transformations chainable and composable as opposed
  -- to depending on case matching blocks. - Parnell
  case (tsb1, blk2) of
    (Nothing, Nothing) ->
      return (Timestamp ts1 act Nothing)
    (Nothing, Just (ts2, Nothing, _)) ->
      return (Timestamp ts1 act (Just ts2))
    (Nothing, Just _) ->
      fail "Illegal time range in second timerange timestamp"
    (Just (h',m'), Nothing) ->
      return (Timestamp ts1 act
               (Just $ ts1 {hourMinute = Just (h',m')
                           ,repeater   = Nothing
                           ,delay      = Nothing}))
    (Just _, Just _) -> fail "Illegal mix of time range and timestamp range"

  where
    optionalBracketedDateTime =
      option Nothing (Just <$> (string "--" *> parseBracketedDateTime))

type Weekday = Text

data BracketedDateTime =
     BracketedDateTime
     { datePart    :: YearMonthDay
     , dayNamePart :: Maybe Weekday
     , timePart    :: Maybe TimePart
     , repeat      :: Maybe Repeater
     , delayPart   :: Maybe Delay
     , isActive    :: Bool
     } deriving (Show, Eq)

-- | Parse a single time part.
--
-- > [2015-03-27 Fri 10:20 +4h]
--
-- Returns:
--
-- - The basic timestamp
-- - Whether there was a time interval in place of a single time
-- (this will be handled upstream by parseTimestamp)
-- - Whether the time is active or inactive
parseBracketedDateTime :: TP.Parser Text BracketedDateTime
parseBracketedDateTime = do
  openingBracket <- char '<' <|> char '['
  brkDateTime    <- BracketedDateTime <$>
            parseDate <* skipSpace
        <*> optionalParse parseDay
        <*> optionalParse parseTime'
        <*> maybeListParse parseRepeater
        <*> maybeListParse parseDelay
        <*> pure (activeBracket openingBracket)

  closingBracket <- char '>' <|> char ']'
  finally brkDateTime openingBracket closingBracket
  where
    optionalParse p  = option Nothing (Just <$> p) <* skipSpace
    maybeListParse p = listToMaybe <$> many' p  <* skipSpace
    activeBracket = (=='<')

    finally bkd ob cb | complementaryBracket ob /= cb =
                          fail "Mismatched timestamp brackets"
                      | otherwise = return bkd

    complementaryBracket '<' = '>'
    complementaryBracket '[' = ']'
    complementaryBracket x   = x

-- TODO: this function is also grody but it's better than having this
-- logic in the primary parseBracketedDateTime function. - Parnell
transformBracketedDateTime :: BracketedDateTime -> (DateTime, Maybe (Hours, Minutes), Bool)
transformBracketedDateTime
  (BracketedDateTime dp dn tp rp dly act) =
    case tp of
        Just (TimePart (Left (h,m))) ->
            ( DateTime (YMD' dp) dn (Just (h,m)) rp dly
            , Nothing
            , act)
        Just (TimePart (Right (t1, t2))) ->
            ( DateTime (YMD' dp) dn (Just t1) rp dly
            , Just t2
            , act)
        Nothing ->
            ( DateTime (YMD' dp) dn Nothing rp dly
            , Nothing
            , act)

-- | Parse a day name in the same way as org-mode does.
parseDay :: TP.Parser Text Text
parseDay = pack <$> some (TP.satisfyElem isDayChar)
  where
    isDayChar :: Char -> Bool
    isDayChar c = not (c `elem` "]+0123456789>\r\n -")
    -- The above syntax is based on [^]+0-9>\r\n -]+
    -- a part of regexp named org-ts-regexp0
    -- in org.el .



type AbsoluteTime   = (Hours, Minutes)
type TimestampRange = (AbsoluteTime, AbsoluteTime)

newtype TimePart = TimePart (Either AbsoluteTime TimestampRange)
  deriving (Eq, Ord, Show)

-- | Parse the time-of-day part of a time part, as a single point or a
-- time range.
parseTime' :: TP.Parser Text TimePart
parseTime' =
    TimePart <$> choice [ Right <$> ((,) <$> parseHM <* char '-' <*> parseHM)
                        , Left  <$> parseHM
                        ]

-- | Parse the YYYY-MM-DD part of a time part.
parseDate :: TP.Parser Text YearMonthDay
parseDate = consumeDate >>= either failure success . dateParse
  where
    failure e = fail . unpack $ unwords ["Failure parsing date: ", pack e]
    success t = return $ buildTime t
    consumeDate  = manyTill anyChar (char ' ')
    dateParse    = AB.parseOnly dpCombinator . BS.pack
    dpCombinator = timeParser defaultTimeLocale "%Y-%m-%d"

-- | Parse a single @HH:MM@ point.
parseHM :: TP.Parser Text (Hours, Minutes)
parseHM = (,) <$> decimal <* char ':' <*> decimal

-- | Parse the Timeunit part of a delay or repeater flag.
parseTimeUnit :: TP.Parser Text TimeUnit
parseTimeUnit =
    choice [ char 'h' *> pure UnitHour
           , char 'd' *> pure UnitDay
           , char 'w' *> pure UnitWeek
           , char 'm' *> pure UnitMonth
           , char 'y' *> pure UnitYear
           ]

-- | Parse a repeater flag, e.g. @.+4w@, or @++1y@.
parseRepeater :: TP.Parser Text Repeater
parseRepeater =
    Repeater
    <$> choice[ string "++" *> pure RepeatCumulate
              , char   '+'  *> pure RepeatCatchUp
              , string ".+" *> pure RepeatRestart
              ]
    <*> decimal
    <*> parseTimeUnit

-- | Parse a delay flag, e.g. @--1d@ or @-2w@.
parseDelay :: TP.Parser Text Delay
parseDelay =
    Delay
    <$> choice [ string "--" *> pure DelayFirst
               , char '-'    *> pure DelayAll
               ]
    <*> decimal
    <*> parseTimeUnit
