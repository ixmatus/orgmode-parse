-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Headings
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode active and inactive timestamps.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.OrgMode.Parse.Attoparsec.Time
( module Data.OrgMode.Parse.Internal
, timestamp
, scheduled
)
where

import           Control.Applicative         ((*>), (<$>), (<*), (<|>))
import           Data.Attoparsec.Text        as T
import           Data.Attoparsec.Types       as TP (Parser)
import           Data.Char                   (isUpper)
import           Data.Maybe                  (catMaybes, isJust)
import           Data.Text                   as Text (Text, concat, length,
                                                      null, pack)
import           Data.Thyme.Format           (buildTime, timeParser)
import           Data.Thyme.LocalTime        (LocalTime (..))
import           Prelude                     hiding (concat, null, takeWhile)
import           System.Locale               (defaultTimeLocale,
                                              iso8601DateFormat)

import           Data.OrgMode.Parse.Internal

activeTimestamp :: TP.Parser Text LocalTime
activeTimestamp = do
    s'    <- skipSpace *> scheduled
    -- TODO: need to convert the Parser type from ByteString specific
    -- to Text!
    stamp <- skipSpace *> timeParser defaultTimeLocale formatString
    return $ buildTime stamp
  where
    formatString = "%Y-%m-%d %a"

scheduled :: TP.Parser Text (Maybe Scheduled)
scheduled = do
    sd <- option Nothing sched <* char ':'
    return $ case sd of
        Just "DEADLINE"  -> Just DEADLINE
        Just "SCHEDULED" -> Just SCHEDULED
        Nothing          -> Nothing
  where
    sched = Just <$> (string "DEADLINE" <|> string "SCHEDULED")
