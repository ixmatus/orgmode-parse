-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Time
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode timestamps; both active and
-- inactive.
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.OrgMode.Parse.Attoparsec.Time
( parsePlannings
, parseClock
, parseTimestamp
)
where

import           Control.Applicative        
import qualified Data.Attoparsec.ByteString as Attoparsec.ByteString
import           Data.Attoparsec.Combinator as Attoparsec
import           Data.Attoparsec.Text
import           Data.Attoparsec.Types      as Attoparsec (Parser)
import qualified Data.ByteString.Char8      as BS
import           Data.Functor               (($>))
import           Data.HashMap.Strict        (HashMap, fromList)
import           Data.Maybe                 (listToMaybe)
import           Data.Monoid                ()
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Thyme.Format          (buildTime, timeParser)
import           Data.Thyme.LocalTime       (Hours, Minutes)
import           System.Locale              (defaultTimeLocale)
import           Data.Semigroup             ((<>))
import           Data.OrgMode.Types
import qualified Data.OrgMode.Parse.Attoparsec.Util as Util

-- | Parse a planning line.
--
-- Plannings inhabit a heading section and are formatted as a keyword
-- and a timestamp. There can be more than one, but they are all on
-- the same line e.g:
--
-- > DEADLINE: <2015-05-10 17:00> CLOSED: <2015-04-1612:00>
parsePlannings :: Attoparsec.Parser Text (HashMap PlanningKeyword Timestamp)
parsePlannings = fromList <$> many' (skipSpace *> planning <* Util.skipOnlySpace)
  where
    planning =  (,) <$> pType <* char ':' <*> (skipSpace *> parseTimestamp)
    pType    = choice [string "SCHEDULED" $>  SCHEDULED
                      ,string "DEADLINE"  $>  DEADLINE
                      ,string "CLOSED"    $>  CLOSED
                      ]

-- | Parse a clock line.
--
-- A heading's section contains one line per clock entry. Clocks may
-- have a timestamp, a duration, both, or neither e.g.:
--
-- > CLOCK: [2014-12-10 Fri 2:30]--[2014-12-10 Fri 10:30] => 08:00
parseClock :: Attoparsec.Parser Text Clock
parseClock = Clock <$> ((,) <$> (skipSpace *> string "CLOCK: " *> ts) <*> dur)
  where
    ts  = optional parseTimestamp
    dur = optional (string " => " *> skipSpace *> parseHM)

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
parseTimestamp :: Attoparsec.Parser Text Timestamp
parseTimestamp = do
  (ts1, tsb1, act) <- transformBracketedDateTime <$> parseBracketedDateTime

  blk2 <- fmap (fmap transformBracketedDateTime)  optionalBracketedDateTime

  -- TODO: refactor this case logic
  case (tsb1, blk2) of
    (Nothing, Nothing) ->
      pure (Timestamp ts1 act Nothing)
    (Nothing, Just (ts2, Nothing, _)) ->
      pure (Timestamp ts1 act (Just ts2))
    (Nothing, Just _) ->
      -- TODO: improve error message with an example of what would
      -- cause this case
      fail "Illegal time range in second timerange timestamp"
    (Just (h',m'), Nothing) ->
      pure (Timestamp ts1 act
               (Just $ ts1 {hourMinute = Just (h',m')
                           ,repeater   = Nothing
                           ,delay      = Nothing}))
    (Just _, Just _) ->
      -- TODO: improve error message with an example of what would
      -- cause thise case
      fail "Illegal mix of time range and timestamp range"

  where
    optionalBracketedDateTime =
      optional (string "--" *> parseBracketedDateTime)


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
parseBracketedDateTime :: Attoparsec.Parser Text BracketedDateTime
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
    optionalParse p  = optional p <* skipSpace
    maybeListParse p = listToMaybe <$> many' p  <* skipSpace
    activeBracket ((=='<') -> active) =
      if active then Active else Inactive

    finally bkd ob cb | complementaryBracket ob /= cb =
                          -- TODO: improve this error message with an
                          -- example of what would cause this case
                          fail "mismatched timestamp brackets"
                      | otherwise = return bkd

    complementaryBracket '<' = '>'
    complementaryBracket '[' = ']'
    complementaryBracket x   = x

-- | Given a @BracketedDateTime@ data type, transform it into a triple
-- composed of a @DateTime@, possibly a @(Hours, Minutes)@ tuple
-- signifying the end of a timestamp range, and a boolean indic
transformBracketedDateTime :: BracketedDateTime
                           -> (DateTime, Maybe (Hours, Minutes), ActiveState)
transformBracketedDateTime BracketedDateTime{..} =
  maybe dateStamp timeStamp timePart
  where
    defdt = DateTime datePart dayNamePart Nothing repeat delayPart
    timeStamp (AbsoluteTime   (hs,ms)) =
      ( defdt { hourMinute = Just (hs,ms) }
      , Nothing
      , activeState
      )
    timeStamp (TimeStampRange (t0,t1)) =
      ( defdt { hourMinute = Just t0 }
      , Just t1
      , activeState
      )
    dateStamp = (defdt, Nothing, activeState)


-- | Parse a day name in the same way as org-mode does.
--
-- The character set (@]+0123456789>\r\n -@) is based on a part of a
-- regexp named @org-ts-regexp0@ found in org.el.
parseDay :: Attoparsec.Parser Text Text
parseDay = Text.pack <$> some (Attoparsec.satisfyElem isDayChar)
  where
    isDayChar :: Char -> Bool
    isDayChar = (`notElem` nonDayChars)

    -- | This is based on: @[^]+0-9>\r\n -]+@, a part of a regexp
    -- named org-ts-regexp0 in org.el.
    nonDayChars = "]+0123456789>\r\n -" :: String

-- | Parse the time-of-day part of a time part, as a single point or a
-- time range.
parseTime' :: Attoparsec.Parser Text TimePart
parseTime' = stampRng <|> stampAbs
  where
    stampRng = do
      beg <- parseHM <* char '-'
      end <- parseHM
      pure $ TimeStampRange (beg,end)

    stampAbs = AbsoluteTime   <$> parseHM

-- | Parse the YYYY-MM-DD part of a time part.
parseDate :: Attoparsec.Parser Text YearMonthDay
parseDate = consumeDate >>= either bad good . dateParse
  where
    bad e        = fail $ "failure parsing date: " <> e
    good t       = pure $ buildTime t
    consumeDate  = manyTill anyChar $ char ' '
    dateParse    = Attoparsec.ByteString.parseOnly dpCombinator . BS.pack
    dpCombinator = timeParser defaultTimeLocale "%Y-%m-%d"

-- | Parse a single @HH:MM@ point.
parseHM :: Attoparsec.Parser Text (Hours, Minutes)
parseHM = (,) <$> decimal <* char ':' <*> decimal

-- | Parse the Timeunit part of a delay or repeater flag.
parseTimeUnit :: Attoparsec.Parser Text TimeUnit
parseTimeUnit =
  choice [ char 'h' $> UnitHour
         , char 'd' $> UnitDay
         , char 'w' $> UnitWeek
         , char 'm' $> UnitMonth
         , char 'y' $> UnitYear
         ]

-- | Parse a repeater flag, e.g. @.+4w@, or @++1y@.
parseRepeater :: Attoparsec.Parser Text Repeater
parseRepeater =
  Repeater
  <$> choice
        [ string "++" $> RepeatCumulate
        , char   '+'  $> RepeatCatchUp
        , string ".+" $> RepeatRestart
        ]
  <*> decimal
  <*> parseTimeUnit

-- | Parse a delay flag, e.g. @--1d@ or @-2w@.
parseDelay :: Attoparsec.Parser Text Delay
parseDelay =
  Delay
  <$> choice
        [ string "--" $> DelayFirst
        , char   '-'  $> DelayAll
        ]
  <*> decimal
  <*> parseTimeUnit
