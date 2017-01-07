-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.PropertyDrawer
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode section property drawers.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.PropertyDrawer
( parseDrawer
, property
, PropertyKey
, PropertyVal
)
where

import           Control.Applicative      ((*>), (<*))
import           Data.Attoparsec.Text     as T
import           Data.Attoparsec.Types    as Attoparsec
import           Data.HashMap.Strict      (fromList)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Prelude                  hiding (concat, null, takeWhile)

import           Data.OrgMode.Parse.Types

type PropertyKey = Text
type PropertyVal = Text

-- | Parse a property drawer.
--
-- > :PROPERTIES:
-- > :DATE: [2014-12-14 11:00]
-- > :NOTE: Something really crazy happened today!
-- > :END:
parseDrawer :: Attoparsec.Parser Text Properties
parseDrawer = pure . fromList =<< begin *> manyTill property end
  where
    begin   = parseDelim "PROPERTIES"
    end     = parseDelim "END"

parseDelim :: Text -> Attoparsec.Parser Text Text
parseDelim v =
  skipSpace *> skip (== ':') *>
  asciiCI v                  <*
  skip (== ':') <* skipSpace

-- | Parse a property of a drawer.
--
-- Properties *must* be a `:KEY: value` pair, the key can be of any
-- case and contain any characters except for newlines and colons
-- (since they delimit the start and end of the key).
property :: Attoparsec.Parser Text (PropertyKey, PropertyVal)
property = (,) <$> parseKey <*> parseVal
  where
    parseKey  = skipSpace *> skip (== ':') *> takeWhile1 (/= ':') <* skip (== ':')
    parseVal  = Text.strip <$> (skipSpace *> takeValue)
    takeValue = takeWhile1 (not . isEndOfLine)
