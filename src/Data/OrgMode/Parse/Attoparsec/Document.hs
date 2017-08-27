-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Document
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Top-level attoparsec parser for org-mode documents.
----------------------------------------------------------------------------

module Data.OrgMode.Parse.Attoparsec.Document
( parseDocument
)
where

import           Control.Applicative                    ((<$>), (<*>))
import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                  as Attoparsec
import           Data.Text                              (Text)
import qualified Data.Text                              as Text

import           Data.OrgMode.Parse.Attoparsec.Headline
import qualified Data.OrgMode.Parse.Attoparsec.Util     as Util
import           Data.OrgMode.Types

------------------------------------------------------------------------------
parseDocument :: [Text] -> Attoparsec.Parser Text Document
parseDocument otherKeywords =
  Document
    <$> (Text.unlines <$> many' Util.nonHeadline)
    <*> many' (headlineBelowDepth otherKeywords 0)
