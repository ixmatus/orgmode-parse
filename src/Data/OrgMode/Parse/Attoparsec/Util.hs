{-|
Module      :  Data.OrgMode.Parse.Attoparsec.Util
Copyright   :  © 2017 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Attoparsec utilities.
-}

{-# LANGUAGE TupleSections #-}

module Data.OrgMode.Parse.Attoparsec.Util
( skipOnlySpace,
  nonHeadline,
  takeALine,
  takeLinesTill,
  isHeadLine,
  takeContentBeforeBlockTill,
  takeEmptyLine
)
where

import           Data.Semigroup 
import qualified Data.Attoparsec.Text  as Attoparsec.Text
import           Data.Attoparsec.Text  (Parser, takeTill, isEndOfLine, anyChar, endOfLine, notChar, skipSpace, option, isHorizontalSpace)
import           Data.Text             (Text, cons, snoc, find)
import qualified Data.Text             as Text
import           Data.Char             (isSpace)
import           Data.Functor          (($>))
import           Data.Maybe            (isNothing)
-- | Skip whitespace characters, only!
--
-- @Data.Attoparsec.Text.skipSpace@ uses the @isSpace@ predicate from
-- @Data.Char@ which also includes control characters such as a return
-- and newline which we need to *not* consume in some cases during
-- parsing.
skipOnlySpace :: Parser ()
skipOnlySpace = Attoparsec.Text.skipWhile isHorizontalSpace

-- | Parse a non-heading line of a section.
nonHeadline :: Parser Text
nonHeadline = nonHeadline0 <> nonHeadline1
  where
    nonHeadline0 = endOfLine $> Text.empty
    nonHeadline1 = cons <$> notChar '*' <*> (takeTill isEndOfLine <* endOfLine)

takeALine :: Parser Text
takeALine = do
  content <- takeTill isEndOfLine
  Attoparsec.Text.option content (snoc content <$> anyChar)

takeLinesTill :: (Text -> Bool) -> Parser Text
takeLinesTill p = fst <$> Attoparsec.Text.match takePLines where
  takePLines = takeALine >>= continueALine
  continueALine content
    | isEmptyLine content = return ()
    | p content           = fail ""
    | otherwise = takePLines <> return ()

-- Whether the content is ended by *text* or :text:, is used to handle isDrawer and isHeadLine
isLastSurroundBy :: Char -> Text -> Bool
isLastSurroundBy c content = case Text.split ( == c) content of
      [_, x, y] -> (not . Text.null) x && (Text.null y || Text.all isSpace y)
      _ -> False

isHeadLine :: Text -> Bool
isHeadLine content = (not . Text.null) content && Text.head content == '*' && not (isLastSurroundBy '*' content)

isEmptyLine :: Text -> Bool
isEmptyLine  = isNothing . find (not . isSpace)
takeEmptyLine :: Parser Text
takeEmptyLine = Attoparsec.Text.takeWhile isHorizontalSpace <* endOfLine

takeContentBeforeBlockTill :: (Text -> Bool) -> Parser s -> Parser (Text, Maybe s)
takeContentBeforeBlockTill p parseBlock = scanBlock where
  scanBlock = ((Text.empty, ) . Just  <$> parseBlock) <> (takeALine >>= appendALine)
  -- Empty line is always an breaker
  appendALine content 
    | isEmptyLine content = return (content, Nothing)
    | p content = fail ""
    | otherwise = do 
      (restContent, block) <- scanBlock <> return (Text.empty, Nothing)
      return (Text.append content restContent, block)
