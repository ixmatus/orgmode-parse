{-|
Module      :  Data.OrgMode.Parse.Attoparsec.Util
Copyright   :  © 2017 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Attoparsec utilities.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Data.OrgMode.Parse.Attoparsec.Util
( skipOnlySpace,
  nonHeadline,
  takeALine,
  isEmptyLine,
  isHeadline,
  parseLinesTill,
  takeBlockBreak,
  skipEmptyLines,
)

where

import qualified Control.Monad
import           Control.Arrow         ((&&&))
import           Data.Semigroup
import qualified Data.Attoparsec.Text  as Attoparsec.Text
import           Data.Attoparsec.Text  (Parser, takeTill, isEndOfLine, many1, anyChar, endOfLine, char, notChar, isHorizontalSpace, atEnd, many', parseOnly)
import           Data.Text             (Text, cons, snoc)
import qualified Data.Text             as Text
import           Data.Functor          (($>))

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

-- | Match only if there are more content to be consumed.
--
-- Opposite of @Attoparsec.Text.endOfInput@
hasMoreInput :: Parser ()
hasMoreInput = do
  x <- atEnd
  Control.Monad.when x $ fail "reach the end of input"

-- | Is the parser parse-able at the current position
resetPosition :: forall a. Parser a -> Parser Bool
resetPosition c = backward where
  c' = (Right <$> c) <> (Left <$> return ())
  p = do
    r <- c'
    case r of
      Right _ -> fail ""
      Left _ -> return False
  backward = p <> return True

-- | Matches only if the incoming text line consists nothing or only spaces
--
-- A empty line always ends a SectionBlock
takeEmptyLine :: Parser Text
takeEmptyLine = Attoparsec.Text.takeWhile isHorizontalSpace <* endOfLine

-- | Whether a text consist only spaces
isEmptyLine :: Parser Bool
isEmptyLine  = resetPosition takeEmptyLine

-- | Is the current line is a Headline
isHeadline :: Parser Bool
isHeadline = resetPosition headline

-- | Succeed if it is a headline
headline :: Parser ()
headline = hasHeadlinePrefix *> atLeastOneSpace where
  atLeastOneSpace :: Parser ()
  atLeastOneSpace = do
    z <- anyChar
    if isHorizontalSpace z
      then return ()
      else fail "Requiring a space after * for headline"
  hasHeadlinePrefix = many1 (char '*')

-- | Is the current line a @SectionBlock@ break.  A Line is a break
takeBlockBreak ::  Parser ()
takeBlockBreak = breakByEmptyLine <> breakByHeadline where
  breakByEmptyLine = takeEmptyLine $> ()
  breakByHeadline = headline $> ()

-- | Transform a text content as block to work with current parser state
feedParserText :: Parser s -> Text -> Parser s
feedParserText  p t =
  case parseOnly p t of
    Left s -> fail s
    Right s -> return s

-- | Save the content and parse as the default Plain Text or default Section Paragraph
-- and try to parse the new block if the new block exists under the same node
parseLinesTill ::  forall a b. Parser a -> Parser (Either b a) -> Parser [a]
parseLinesTill common end = skipEmptyLines *> hasMoreInput *> scanTill where
  stop :: Parser (Either () a)
  stop = do
    z <- (Right <$> end) <> (Left <$> return ())
    case z of
      Left _ -> return (Left ())
      Right (Left _) -> fail ""
      Right (Right x) -> return (Right x)
  takeContent ::  Parser (Text, [a])
  takeContent = tContent <> return (Text.empty, [])
  tContent ::  Parser (Text, [a])
  tContent = do
    z <- stop
    case z of
      Left _ -> (\l -> (Text.append l . fst) &&& snd) <$> takeALine <*> takeContent
      Right a -> return (Text.empty, [a])
  scanTill ::  Parser [a]
  scanTill = do
    (content, blocks) <- takeContent
    (: blocks) <$> feedParserText common content

skipEmptyLines :: Parser ()
skipEmptyLines = many' takeEmptyLine $> ()
