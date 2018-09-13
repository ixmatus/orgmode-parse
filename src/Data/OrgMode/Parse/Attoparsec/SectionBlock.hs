-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.SectionBlock
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markups and paragraphs.
----------------------------------------------------------------------------

{-# LANGUAGE TupleSections        #-}

module Data.OrgMode.Parse.Attoparsec.SectionBlock
(
  parseBlockAndDrawer
)
where

import           Control.Applicative                   ()
import           Data.Semigroup
import           Data.Char                             (isSpace)
import           Data.Text                             (Text)
import qualified Data.Text                      as     Text
import           Data.Attoparsec.Text                  (Parser, many')
import           Data.OrgMode.Types                    (SectionBlock (..), List (..), Paragraph (..))

import           Data.OrgMode.Parse.Attoparsec.Util                (isHeadLine, takeContentBeforeBlockTill, feedParserText, isEmptyLine)
import           Data.OrgMode.Parse.Attoparsec.SectionBlock.Markup    (parseMarkupContent)
import           Data.OrgMode.Parse.Attoparsec.SectionBlock.List      (parseList)

-- | Parse the content until reaching a drawer or a block end;  Try to parse the upcoming drawer
parseBlockAndDrawer :: Parser s -> Parser ([SectionBlock], Maybe s)
parseBlockAndDrawer parseDrawer = do
  (content, drawer) <- takeContentBeforeBlockTill isHeadLine parseDrawer
  if isEmptyLine content
     then return ([], drawer)
     else (, drawer) <$> innerTextToBlocks content

innerTextToBlocks :: Text -> Parser [SectionBlock]
innerTextToBlocks = feedParserText parseBlocks where
  parseBlocks :: Parser [SectionBlock]
  parseBlocks = concat <$> many' parseBlock
  parseBlock =   takeContentBeforeBlockTill isHeadLine parseList >>= appendParagraphAndList
  appendParagraphAndList :: (Text, Maybe List) -> Parser [SectionBlock]
  appendParagraphAndList (text, list) = (++) <$> fetchParagraph text <*> fetchList list
  fetchParagraph :: Text  -> Parser [SectionBlock]
  fetchParagraph content
    | isEmptyLine content = return []
    | otherwise = (: []) . SectionBlock . Right . Paragraph <$> feedParserText parseMarkupContent (Text.dropWhileEnd isSpace content)
  fetchList ::  Maybe List -> Parser [SectionBlock]
  fetchList Nothing = return []
  fetchList (Just x) = return [SectionBlock (Left x)]
