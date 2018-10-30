-----------------------------------------------------------------------------
-- |
-- Module      :  Data.OrgMode.Parse.Attoparsec.Block
-- Copyright   :  © 2014 Parnell Springmeyer
-- License     :  All Rights Reserved
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Parsing combinators for org-mode markup and paragraphs.
----------------------------------------------------------------------------

module Data.OrgMode.Parse.Attoparsec.Block
(
  parseBlocks
)
where

import           Data.Semigroup                        ((<>))
import           Data.Attoparsec.Text                  (Parser, many', eitherP)
import           Data.OrgMode.Types                    (Block (..))

import           Data.OrgMode.Parse.Attoparsec.Util                   (parseLinesTill, takeBlockBreak)
import           Data.OrgMode.Parse.Attoparsec.Block.Paragraph    (parseParagraph)
import           Data.OrgMode.Parse.Attoparsec.Block.List      (parseList)
import           Data.OrgMode.Parse.Attoparsec.Drawer                 (parseDrawer)


-- | Parse the content until reaching a drawer, a list, or a block end.  And include the parsed drawer.
parseBlocks :: Parser [Block]
parseBlocks = concat <$> many' p
  where
  p :: Parser [Block]
  p = do
    blocks <- parseLinesTill parseParagraph (eitherP takeBlockBreak (parseDrawer <> parseList))
    return $ filter (/= Paragraph []) blocks
