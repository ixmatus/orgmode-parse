{-# LANGUAGE OverloadedStrings #-}

module Data.OrgMode.Parse.Attoparsec.Section where

import           Control.Applicative                     ((<$>), (<*), (*>), pure)
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


-- | Parse the state indicator {`TODO` | `DONE` | otherTodoKeywords }.
--
-- These can be custom so we're parsing additional state
-- identifiers as Text
parseTodoKeyword :: [Text] -> TP.Parser Text TodoKeyword
parseTodoKeyword otherKeywords =
    choice ([string "TODO" *> pure TODO
            ,string "DONE" *> pure DONE
            ] ++
            map (\k -> OtherKeyword <$> string k) otherKeywords)


nonHeaderLine :: TP.Parser Text Text
nonHeaderLine = pack <$> manyTill (notChar '*') endOfLine

