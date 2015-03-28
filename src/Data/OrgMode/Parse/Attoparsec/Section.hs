{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Section where

import           Control.Applicative                     ((<$>), (<*))
import           Data.Attoparsec.Text                    as T
import           Data.Attoparsec.Types                   as TP
import           Data.Monoid                             (mempty)
import           Prelude                                 hiding (unlines)
import           Data.Text                               (Text, pack, unlines)
import           Data.OrgMode.Parse.Types
import           Data.OrgMode.Parse.Attoparsec.Time
import           Data.OrgMode.Parse.Attoparsec.PropertyDrawer


parseSection :: TP.Parser Text Section
parseSection = do
  clks  <- many' parseClock
  plns  <- parsePlannings
  props <- option mempty parseDrawer <* skipSpace
  leftovers <- unlines <$> many' nonHeaderLine
  return (Section (Plns plns) props clks leftovers)


nonHeaderLine :: TP.Parser Text Text
nonHeaderLine = pack <$> manyTill (notChar '*') endOfLine

