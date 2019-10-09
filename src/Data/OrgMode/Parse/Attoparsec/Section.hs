-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Section
-- Copyright   :  © 2015 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode headline sections.
----------------------------------------------------------------------------



module Data.OrgMode.Parse.Attoparsec.Section where

import           Control.Applicative                   (optional)
import           Data.Attoparsec.Text                  (many', option,
                                                        skipSpace)
import           Data.Monoid                           ()
import           Data.OrgMode.Parse.Attoparsec.Drawer
import           Data.OrgMode.Parse.Attoparsec.Time
import qualified Data.OrgMode.Parse.Attoparsec.Util   as Util

import           Data.OrgMode.Types

-- | Parse a heading section
--
-- Headline sections contain optionally a property drawer,
-- a list of clock entries, code blocks (not yet implemented),
-- plain lists (not yet implemented), and unstructured text.
parseSection :: Attoparsec.Text.Parser Section
parseSection = skipEmptyLines *> parseSection' <* skipEmptyLines
  where
    parseSection' = Section
     <$> optional (skipSpace *> parseTimestamp <* skipSpace)
     <*> parsePlannings
     <*> many' parseClock
     <*> option mempty parseProperties
     <*> option mempty parseLogbook
     <*> parseContents
