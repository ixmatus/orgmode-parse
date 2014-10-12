-----------------------------------------------------------------------------
-- |
-- Module      :  Data.StructMsg.Attoparsec.Headings
-- Copyright   :  (C) 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-list headings.
----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Data.StructMsg.Attoparsec.Headings where

import           Control.Applicative  ((*>), (<*), (<*>), (<|>))
import           Data.Attoparsec.Text

data Heading = Heading
  { level = Int
  , priority = Priority
  , state    = Text
  , title    = Text
  , keywords = [Keyword]
  } deriving (Show, Eq)


headingLevel = many1 $ char '*'
