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

import           Control.Applicative        ((<*>), (*>), (<$>), (<*),
                                             (<|>), pure)
import           Control.Monad              (when)
import qualified Data.Attoparsec.ByteString as AB
import           Data.Attoparsec.Text       as T
import           Data.Attoparsec.Types      as TP (Parser)
import qualified Data.ByteString.Char8      as BS
import           Data.HashMap.Strict        (HashMap,fromList)
import           Data.Maybe                 (listToMaybe)
import           Data.Text                  as Text (Text, unwords,
                                                     pack, unpack )
import           Data.Thyme.Format          (buildTime, timeParser)
import           Data.Thyme.LocalTime       (Hours, Minutes)
import           Prelude                    hiding (concat, null,
                                                    takeWhile,
                                                    unwords, words)
import           System.Locale              (defaultTimeLocale)

import           Data.OrgMode.Parse.Types

-- | Parse a planning line
--
-- Plannings live in a heading section and are formatted as a keyword and a timestamp
-- There can be more than one, but they are all on the same line
-- e.g. DEADLINE: <2015-05-10 17:00> CLOSED: <2015-04-16 12:00>
parsePlannings :: TP.Parser Text (HashMap PlanningKeyword Timestamp)
parsePlannings = fromList <$> (many' (planning <* skipSpace))
  where planning :: TP.Parser Text (PlanningKeyword, Timestamp)
        planning =  (,) <$> pType <* char ':' <*> (skipSpace *> parseTimestamp)
        pType    = choice [string "SCHEDULED" *> pure SCHEDULED
                          ,string "DEADLINE"  *> pure DEADLINE
                          ,string "CLOSED"    *> pure CLOSED
                          ]

-- | Parse a clock line
--
-- A heading's section contains one line per clock entry
-- Clocks may have a timestamp, a duration, both, or neither
-- e.g.: CLOCK: [2014-12-10 Fri 2:30]--[2014-12-10 FRI 10:30]  => 08:00
parseClock :: TP.Parser Text (Maybe Timestamp, Maybe Duration)
parseClock = (,) <$> (skipSpace *> string "CLOCK: " *> ts) <*> dur
  where ts  = option Nothing (Just <$> parseTimestamp)
        dur = option Nothing (Just <$> (string " => "
                              *> skipSpace *> parseHM))

-- | Parse a timestamp
--
-- Timestamps may be timepoints or timeranges, and they indicate
-- whether they are active or closed by using angle or square brackets respectively
--
-- Time ranges are formatted by appending two timepoints with '--' in between,
--   or by appending two hh:mm stamps together in a single timepoint with a single '-'
--
-- Each timepoint includes an optional repeater flag and an optional delay flag
parseTimestamp :: TP.Parser Text Timestamp
parseTimestamp = do
  (ts1,ts1b,active1) <- parseBracketedDateTime
  t2 <- option Nothing (Just <$> (string "--" *> parseBracketedDateTime))
  case (ts1b,t2) of
    (Nothing,      Nothing) ->
       return (Timestamp ts1 active1 Nothing)
    (Nothing, Just (ts2,Nothing,_)) ->
      return  (Timestamp ts1 active1 (Just ts2))
    (Nothing, Just _) ->
      fail "Illegal time range in second timerange timestamp"
    (Just (h',m'), Nothing) ->
      return (Timestamp ts1 active1
               (Just $ ts1 {hourMinute = Just (h',m')
                           ,repeater   = Nothing
                           ,delay      = Nothing}))
    (Just _, Just _) -> fail "Illegal mix of time range and timestamp range"


-- | Parse a single time part
--
-- e.g. [2015-03-27 Fri 10:20 +4h]
-- returning:
--   * The basic timestamp
--   * Whether there was a time interval in place of a single time
--       (this will be handled upstream by parseTimestamp)
--   * Whether the time is active or inactive
parseBracketedDateTime :: TP.Parser Text (DateTime, Maybe (Hours,Minutes), Bool)
parseBracketedDateTime = do
  oBracket   <- char '<' <|> char '['
  datePart   <- parseDate                            <* skipSpace
  dName      <- option Nothing (Just <$> parseDay)   <* skipSpace
  timePart   <- option Nothing (Just <$> parseTime') <* skipSpace
  repeatPart <- listToMaybe <$> many' parseRepeater  <* skipSpace
  delayPart  <- listToMaybe <$> many' parseDelay     <* skipSpace
  cBracket   <- char '>' <|> char ']'

  when (complementaryBracket oBracket /= cBracket)
    (fail "Bad timestamp parse, mismatched brackets")

  case timePart of
    Just (Left (h,m)) ->
      return ( DateTime (YMD' datePart) dName (Just (h,m)) repeatPart delayPart
             , Nothing
             , activeBracket '<')
    Just (Right (t1,t2)) ->
      return ( DateTime (YMD' datePart) dName (Just t1)repeatPart delayPart
             , Just t2
             , activeBracket '<')
    Nothing -> fail "Failed to parse a timestamp (HH:MM)"

    where activeBracket = (=='<')
          complementaryBracket '<' = '>'
          complementaryBracket '[' = ']'
          complementaryBracket x   = x

-- | Parse a 3-character day name
parseDay :: TP.Parser Text Text
parseDay = choice (map string ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"])

-- | Parse the time-of-day part of a time part, as a single point or a time range
parseTime' :: TP.Parser Text (Either (Hours,Minutes) ((Hours,Minutes),(Hours,Minutes)))
parseTime' = choice [Right <$> ((,) <$> parseHM <* char '-' <*> parseHM)
                    ,Left  <$> parseHM ]


-- | Parse the YYYY-MM-DD part of a time part
parseDate :: TP.Parser Text YearMonthDay
parseDate = manyTill anyChar (char ' ') >>= \dStr ->
  case AB.parseOnly dParser (BS.pack dStr) of
    Left e      -> fail .unpack $
                   unwords ["No time parse for: ", pack dStr, " Message:", pack e]
    Right lTime -> return (buildTime lTime)
  where dParser = timeParser defaultTimeLocale "%Y-%m-%d"

-- | Parse a single HH:MM point
parseHM :: TP.Parser Text (Hours,Minutes)
parseHM = (,) <$> decimal <* char ':' <*> decimal

-- Parse the Timeunit part of a delay or repeater flag
parseTimeUnit :: TP.Parser Text TimeUnit
parseTimeUnit = choice [char 'h' *> pure UnitHour
                       ,char 'd' *> pure UnitDay
                       ,char 'w' *> pure UnitWeek
                       ,char 'm' *> pure UnitMonth
                       ,char 'y' *> pure UnitYear]


-- | Parse a repeater flag, e.g. ".+4w", or "++1y"
parseRepeater :: TP.Parser Text Repeater
parseRepeater = Repeater
                <$> choice[string "++" *> pure RepeatCumulate
                          ,string "+"  *> pure RepeatCatchUp
                          ,string ".+" *> pure RepeatRestart
                          ]
                <*> decimal
                <*> parseTimeUnit

-- | Parse a delay flag, e.g. "--1d" or "-2w"
parseDelay :: TP.Parser Text Delay
parseDelay = Delay
             <$> choice [string "--" *> pure DelayFirst
                        ,string "-"  *> pure DelayAll
                        ]
             <*> decimal
             <*> parseTimeUnit

