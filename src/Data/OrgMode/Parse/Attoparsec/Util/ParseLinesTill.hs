{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      :  Data.OrgMode.Parse.Attoparsec.Util.ParseLinesTill
Copyright   :  © 2017 Parnell Springmeyer
License     :  All Rights Reserved
Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
Stability   :  stable

Attoparsec utilities
-}
module Data.OrgMode.Parse.Attoparsec.Util.ParseLinesTill (
    takeALine,
    ParseLinesTill (..),
    takeContentBreak,
    skipEmptyLines,
)
where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Attoparsec.Text (
    Parser,
    anyChar,
    atEnd,
    char,
    endOfLine,
    isEndOfLine,
    isHorizontalSpace,
    many1,
    parseOnly,
    takeTill,
    (<?>),
 )
import Data.Foldable (Foldable (..))
import Data.Functor (($>))
import Data.Text (Text, snoc)

import qualified Control.Monad
import qualified Data.Attoparsec.Text as Attoparsec.Text
import qualified Data.Text as Text

takeALine :: Parser Text
takeALine = do
    content <- takeTill isEndOfLine
    Attoparsec.Text.option content (snoc content <$> anyChar)

{- | Match only if there are more content to be consumed.

Opposite of @Attoparsec.Text.endOfInput@
-}
hasMoreInput :: Parser ()
hasMoreInput = do
    x <- atEnd
    Control.Monad.when x $ fail "reached the end of input"

{- | Matches only if the incoming text line consists nothing or only spaces

A empty line always ends a SectionContent
-}
takeEmptyLine :: Parser Text
takeEmptyLine = Attoparsec.Text.takeWhile isHorizontalSpace <* endOfLine

-- | Succeed if it is a headline
headline :: Parser ()
headline = hasHeadlinePrefix *> atLeastOneSpace
  where
    hasHeadlinePrefix = many1 (char '*')

    atLeastOneSpace :: Parser ()
    atLeastOneSpace = do
        z <- anyChar
        guard (isHorizontalSpace z) <?> "A space must follow the last * of a headline"

-- | Is the current line a @SectionContent@ break.  A Line is a break
takeContentBreak :: Parser ()
takeContentBreak = breakByEmptyLine <> headline
  where
    breakByEmptyLine = takeEmptyLine $> ()

-- | Transform a text content as block to work with current parser state
feedParserText :: Parser s -> Text -> Parser s
feedParserText p t =
    case parseOnly p t of
        Left s -> fail s
        Right s -> return s

type Recursive m b a = b -> (b, Parser (m a))

class (Foldable m) => ParseLinesTill m where
    -- | Fail and reset position when a breaker is found
    stop :: forall a. Parser (m a) -> Parser (Either () [a])
    stop p' =
        hasMoreInput *> do
            z <- (Right <$> p') <> (return . Left) ()
            case z of
                Left _ -> return (Left ())
                Right x -> guard ((not . null) x) $> (Right . toList $ x)

    takeContent :: forall a b. Recursive m b a -> b -> Parser (Text, [a])
    takeContent next c = tContent <> return (Text.empty, [])
      where
        (c', p) = next c
        tContent = do
            z <- stop p
            case z of
                Left _ -> (\l -> (Text.append l . fst) &&& snd) <$> takeALine <*> takeContent next c'
                Right as -> return (Text.empty, as)

    -- | Save the content and parse as the default Plain Text or default Section Paragraph
    -- and try to parse the new block if the new block exists under the same node
    parseLinesContextuallyTill :: forall a b. Parser a -> Recursive m b a -> b -> Parser [a]
    parseLinesContextuallyTill pD next c =
        skipEmptyLines *> hasMoreInput *> do
            (content, blocks) <- takeContent next c
            guard (not $ Text.null content && null blocks) *> ((: blocks) <$> feedParserText pD content)

    parseLinesTill :: forall a. Parser a -> Parser (m a) -> Parser [a]
    parseLinesTill pDefault pBreaker = parseLinesContextuallyTill pDefault (const (0 :: Integer, pBreaker)) 0

instance ParseLinesTill (Either a)

skipEmptyLines :: Parser ()
skipEmptyLines = Attoparsec.Text.many' takeEmptyLine $> ()
