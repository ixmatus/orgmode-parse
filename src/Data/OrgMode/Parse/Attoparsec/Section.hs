-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Section
-- Copyright   :  © 2015 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode sections.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Section where

import           Control.Applicative                          ((<$>), (<*>), (<|>))
import           Data.Attoparsec.Text                         as T
import           Data.Attoparsec.Types                        as TP
import           Data.Monoid                                  (mempty)
import           Data.Text                                    (Text, pack,
                                                               unlines)
import           Prelude                                      hiding (unlines)

import           Data.OrgMode.Parse.Attoparsec.PropertyDrawer
import           Data.OrgMode.Parse.Attoparsec.Time
import           Data.OrgMode.Parse.Types

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

-- | Parse a non-heading line of a section.
nonHeaderLine :: TP.Parser Text Text
nonHeaderLine = nonHeaderLine0 <|> nonHeaderLine1
  where
    nonHeaderLine0 = endOfLine >> return (pack "")
    nonHeaderLine1 = pack <$> do
      h <- notChar '*'
      t <- manyTill anyChar endOfLine
      return (h:t)
