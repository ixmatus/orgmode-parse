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
import           Data.Maybe                 (listToMaybe, fromJust, isJust)
import           Data.Text                  as Text (Text, isPrefixOf, unwords,
                                                     pack, unpack, words)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Thyme.Calendar        (YearMonthDay(..))
import           Data.Thyme.Format          (buildTime, timeParser)
import           Data.Thyme.LocalTime       (Hours, Minutes)
import           Prelude                    hiding (concat, null, takeWhile,
                                             unwords, words)
import           System.Locale              (defaultTimeLocale)

import           Data.OrgMode.Parse.Types

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
      return ( DateTime datePart dName (Just (h,m)) repPart delPart
             , Nothing
             , activeBracket '<')
    Just (Right (t1,t2)) ->
      return ( DateTime datePart dName (Just t1)repPart delPart
             , Just t2
             , activeBracket '<')

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

-- -- | Parse org-mode Timestamps
-- parseTimestamp :: TP.Parsere Text Timestamp
-- -- | Parse an org-mode timestamp (eg "[2015-03-21 Sat 09:45]") with
-- -- user-supplied opening and ending brackets
-- parseTimestamp :: Open -> Close -> TP.Parser Text Timestamp
-- parseTimestamp (Open s) (Close e) = activeState <$>
--   char s *>
--   (timeParser1 <|> timeParser2)
--   <* AB.many' (skipSpace *> parseRecur)
--   <* char e
--   where
--     activeState = if s == '<' && e == '>'
--                   then Active else Inactive
--     timeParser1 = timeParser defaultTimeLocale "%Y-%m-%d %a %H:%M"
--     timeParser2 = timeParser defaultTimeLocale "%Y-%m-%d %a"
-- --      dropRecur   = unwords . filter (not . isPrefixOf "+") . words
--     parseRecur  = char "+" *> AB.many' (digit <|> oneOf "ymdwhm") -- TODO
--     oneOf xs    = AB.choice (map char xs)

-- | Parse an org-mode timestamp line with user-supplied opening and
-- ending brackets (for either active or inactive stamps).
parseTimestampLine :: Open -> Close -> TP.Parser Text (Maybe Schedule)
parseTimestampLine (Open s) (Close e) = do
    s'    <- skipSpace *> scheduleType
    stamp <- skipSpace *> char s *> takeTill (== e) <* char e

    let parts  = words stamp
        r      = if recur parts then Just $ last parts else Nothing
        parsed = undefined
        stamp' = undefined -- Drop code refering to old Schedule structure, allow to compile
        sched  = undefined -- ^

    if isJust $ timestamp sched
    then return $ Just sched
    else return Nothing

  where
    recur p = isPrefixOf "+" $ last p

    stitch p | recur p   = unwords $ init p
             | otherwise = unwords p

    timeParser1 = timeParser defaultTimeLocale "%Y-%m-%d %a %H:%M"
    timeParser2 = timeParser defaultTimeLocale "%Y-%m-%d %a"

    parseStamp p = either (return Nothing) (fmap buildTime) $
      AB.parseOnly
        (option Nothing (Just <$> (timeParser1 <|> timeParser2)))
        (encodeUtf8 $ stitch p)

-- | Parse the type of schedule.
scheduleType :: TP.Parser Text ScheduleType
scheduleType = do
    sd <- option Nothing sched <* skipWhile (== ':')
    return $ case sd of
        Just "DEADLINE"  -> DEADLINE
        Just "SCHEDULED" -> SCHEDULED
        Just _           -> APPOINTMENT
        Nothing          -> APPOINTMENT
  where
    sched = Just <$> (string "DEADLINE" <|> string "SCHEDULED")
