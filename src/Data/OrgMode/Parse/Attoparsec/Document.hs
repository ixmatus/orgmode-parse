module Data.OrgMode.Parse.Attoparsec.Document (
 parseDocument
) where

import           Control.Applicative                    ((<$>), (<*>))
import           Data.Attoparsec.Text
import           Data.Attoparsec.Types                  as TP
import           Data.OrgMode.Parse.Attoparsec.Headline
import           Data.OrgMode.Parse.Attoparsec.Section  (nonHeaderLine)
import           Data.OrgMode.Parse.Types
import           Data.Text                              (Text, unlines)
import           Prelude                                hiding (unlines)

------------------------------------------------------------------------------
parseDocument :: [Text] -> TP.Parser Text Document
parseDocument otherKeywords =
  Document
    <$> (unlines <$> many' nonHeaderLine)
    <*> many' (headingBelowLevel otherKeywords 0)
