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

{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Section where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                        as Attoparsec
import           Data.Monoid
import           Data.Text                                    (Text)
import qualified Data.Text                                    as Text
import           Prelude                                      hiding (unlines)

import           Data.OrgMode.Parse.Attoparsec.PropertyDrawer
import           Data.OrgMode.Parse.Attoparsec.Time
import           Data.OrgMode.Types

-- | Parse a heading section
--
-- Headline sections contain optionally a property drawer,
-- a list of clock entries, code blocks (not yet implemented),
-- plain lists (not yet implemented), and unstructured text.
parseSection :: Attoparsec.Parser Text Section
parseSection =
  Section
   <$> (Plns <$> parsePlannings)
   <*> many' parseClock
   <*> option mempty parseDrawer
   <*> (Text.unlines <$> many' nonHeadline)

-- | Parse a non-heading line of a section.
nonHeadline :: Attoparsec.Parser Text Text
nonHeadline = nonHeadline0 <|> nonHeadline1
  where
    nonHeadline0 = endOfLine *> pure (Text.pack "")
    nonHeadline1 = Text.pack <$> do
      h <- notChar '*'
      t <- manyTill anyChar endOfLine
      pure (h:t)
