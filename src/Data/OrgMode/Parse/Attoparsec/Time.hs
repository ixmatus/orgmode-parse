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

import           Control.Applicative        ((<*>), (*>), (<$>), (<*), (<|>), pure)
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
import           Prelude                    hiding (concat, null, takeWhile,
                                             unwords, words)
import           System.Locale              (defaultTimeLocale)

import           Data.OrgMode.Parse.Types


parsePlannings :: TP.Parser Text (HashMap PlanningKeyword Timestamp)
parsePlannings = fromList <$> (many' (planning <* skipSpace))
  where planning :: TP.Parser Text (PlanningKeyword, Timestamp)
        planning =  (,) <$> pType <* char ':' <*> (skipSpace *> parseTimestamp)
        pType    = choice [string "SCHEDULED" *> pure SCHEDULED
                          ,string "DEADLINE"  *> pure DEADLINE
                          ,string "CLOSED"    *> pure CLOSED
                          ]

parseClock :: TP.Parser Text (Maybe Timestamp, Maybe Duration)
parseClock = (,) <$> (skipSpace *> string "CLOCK: " *> ts) <*> dur
  where ts  = option Nothing (Just <$> parseTimestamp)
        dur = option Nothing (Just <$> (string " => "
                              *> skipSpace *> parseHM))

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

-- Parse a single time part
-- returning:
--   * The basic timestamp
--   * Whether there was a time interval in place of a single time
--       (this will be handled upstream by parseTimestamp)
--   * Whether the time is active or inactive
parseBracketedDateTime :: TP.Parser Text (DateTime, Maybe (Hours,Minutes), Bool)
parseBracketedDateTime = do
  oBracket <- char '<' <|> char '['
  datePart <- parseDate                            <* skipSpace
  dName    <- option Nothing (Just <$> parseDay)   <* skipSpace
  timePart <- option Nothing (Just <$> parseTime') <* skipSpace
  repPart  <- listToMaybe <$> many' parseRepeater  <* skipSpace
  delPart  <- listToMaybe <$> many' parseDelay     <* skipSpace
  cBracket <- char '>' <|> char ']'

  when (complementaryBracket oBracket /= cBracket)
    (fail "Bad timestamp parse, mismatched brackets")

  case timePart of
    Just (Left (h,m)) ->
      return ( DateTime (YMD' datePart) dName (Just (h,m)) repPart delPart
             , Nothing
             , activeBracket '<')
    Just (Right (t1,t2)) ->
      return ( DateTime (YMD' datePart) dName (Just t1)repPart delPart
             , Just t2
             , activeBracket '<')
    Nothing -> fail "Failed to parse a timestamp (HH:MM)"

    where activeBracket = (=='<')
          complementaryBracket '<' = '>'
          complementaryBracket '[' = ']'
          complementaryBracket x   = x

parseDay :: TP.Parser Text Text
parseDay = choice (map string ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"])

parseTime' :: TP.Parser Text (Either (Hours,Minutes) ((Hours,Minutes),(Hours,Minutes)))
parseTime' = choice [Right <$> ((,) <$> parseHM <* char '-' <*> parseHM)
                    ,Left  <$> parseHM ]


parseDate :: TP.Parser Text YearMonthDay
parseDate = manyTill anyChar (char ' ') >>= \dStr ->
  case AB.parseOnly dParser (BS.pack dStr) of
    Left e      -> fail .unpack $
                   unwords ["No time parse for: ", pack dStr, " Message:", pack e]
    Right lTime -> return (buildTime lTime)
  where dParser = timeParser defaultTimeLocale "%Y-%m-%d"


parseHM :: TP.Parser Text (Hours,Minutes)
parseHM = (,) <$> decimal <* char ':' <*> decimal

parseTimeUnit :: TP.Parser Text TimeUnit
parseTimeUnit = choice [char 'h' *> pure UnitHour
                       ,char 'd' *> pure UnitDay
                       ,char 'w' *> pure UnitWeek
                       ,char 'm' *> pure UnitMonth
                       ,char 'y' *> pure UnitYear]

parseRepeater :: TP.Parser Text Repeater
parseRepeater = Repeater
                <$> choice[string "++" *> pure RepeatCumulate
                          ,string "+"  *> pure RepeatCatchUp
                          ,string ".+" *> pure RepeatRestart
                          ]
                <*> decimal
                <*> parseTimeUnit

parseDelay :: TP.Parser Text Delay
parseDelay = Delay
             <$> choice [string "--" *> pure DelayFirst
                        ,string "-"  *> pure DelayAll
                        ]
             <*> decimal
             <*> parseTimeUnit

