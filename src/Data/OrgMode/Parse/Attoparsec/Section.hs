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

import           Control.Applicative                  ()
import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                as Attoparsec
import           Data.Monoid                          ()
import           Data.Text                            (Text)
import qualified Data.Text                            as Text

import           Data.OrgMode.Parse.Attoparsec.Drawer
import           Data.OrgMode.Parse.Attoparsec.Time
import qualified Data.OrgMode.Parse.Attoparsec.Util   as Util
import           Data.OrgMode.Parse.Attoparsec.Paragraph  (parseParagraph)
import           Data.OrgMode.Types

-- | Parse a heading section
--
-- Headline sections contain optionally a property drawer,
-- a list of clock entries, code blocks (not yet implemented),
-- plain lists (not yet implemented), and unstructured text.
parseSection :: Attoparsec.Parser Text Section
parseSection =
  Section
   <$> option Nothing (Just <$> (skipSpace *> parseTimestamp <* skipSpace))
   <*> (Plns <$> parsePlannings)
   <*> many' parseClock
   <*> option mempty parseProperties
   <*> option mempty parseLogbook
   <*> many' parseDrawer
   <*> many' parseParagraph
