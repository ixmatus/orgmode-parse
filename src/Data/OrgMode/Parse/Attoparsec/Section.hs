{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Section where

import           Control.Applicative                     ((<$>), (<*>))
import           Data.Attoparsec.Text                    as T
import           Data.Attoparsec.Types                   as TP
import           Data.Monoid                             (mempty)
import           Prelude                                 hiding (unlines)
import           Data.Text                               (Text, pack, unlines)
import           Data.OrgMode.Parse.Types
import           Data.OrgMode.Parse.Attoparsec.Time
import           Data.OrgMode.Parse.Attoparsec.PropertyDrawer


-- | Parse a heading section
--
-- Heading sections contain optionally a property drawer,
-- a list of clock entries, code blocks (not yet implemented),
-- plain lists (not yet implemented), and unstructured text.
parseSection :: TP.Parser Text Section
parseSection = Section
               <$> (Plns <$> parsePlannings)
               <*> many' parseClock
               <*> option mempty parseDrawer
               <*> (unlines <$> many' nonHeaderLine)
  where
    nonHeaderLine = pack <$> manyTill (notChar '*') endOfLine
