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
, agenda
)
where

import           Control.Applicative         ((*>), (<*), (<|>))
import           Data.Attoparsec.Text        as T
import           Data.Attoparsec.Types       as TP (Parser)
import           Data.Char                   (isUpper)
import           Data.Maybe                  (catMaybes, isJust)
import           Data.Text                   as Text (Text, concat, length,
                                                      null, pack)
import           Prelude                     hiding (concat, null, takeWhile)

import           Data.OrgMode.Parse.Internal
