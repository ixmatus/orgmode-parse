-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Item
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}


module Data.OrgMode.Parse.Attoparsec.SectionBlock.List
( 
  parseList
)
where

import           Control.Applicative                   
import           Data.Semigroup                        
import           Data.Char                             (isSpace)
import           Data.Text                             (Text)
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, (<?>), many1')
import           Data.OrgMode.Types                    (Item (..), List (..))
import           Data.OrgMode.Parse.Attoparsec.Util    (takeALine, takeLinesTill, feedParserText)
import           Data.Maybe                            (isJust)
import           GHC.Generics
import           Data.OrgMode.Parse.Attoparsec.SectionBlock.Markup   (parseMarkupContent)

data ItemStart = ItemStart { prefixLength :: Int, firstLine :: Text} deriving (Show, Eq, Generic)

parseItemStart :: Parser ItemStart
parseItemStart = do
  (prefix, content) <- Text.span isSpace <$> takeALine
  result prefix content where 
      errorMessage =  "not an item start"
      result prefix content = if Text.compareLength prefix 2 == GT
                  then (ItemStart (Text.length prefix) <$> hasFirstLine content) <?> errorMessage
                  else fail errorMessage
      hasFirstLine :: Text -> Parser Text
      hasFirstLine content = if Text.null content || Text.head content /= '*'
                                then fail ""
                                else return $ (Text.strip . Text.tail) content

hasLessPrefixSpaceThen :: Int -> Text -> Bool
hasLessPrefixSpaceThen i str = Text.compareLength str i /= GT || isJust (Text.find (not . isSpace) (Text.take (i + 1) str))

parseItem :: Parser Item
parseItem = do 
  itemStart <- parseItemStart
  textLines <- (Text.append "\n" <$> takeLinesTill (hasLessPrefixSpaceThen (prefixLength itemStart))) <> return ""
  Item <$> feedParserText parseMarkupContent (Text.append (firstLine itemStart) textLines)

-- TODO: Support nested List
parseList :: Parser List
parseList = List <$> many1' parseItem
