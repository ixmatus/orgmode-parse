module Util where

import           Data.Attoparsec.Text    (Parser, parseOnly)
import           Data.Either
import           Data.Text               (pack, Text)
import           Test.HUnit

testParser :: Parser a -> String -> Assertion
testParser f v = fromEither (parseOnly f $ pack v)

expectParse :: (Eq a, Show a) => Parser a -- ^ Parser under test
                              -> Text             -- ^ Message under test
                              -> Either String a  -- ^ Expected parse result
                              -> Assertion
expectParse p t (Left _)  = assertBool "Expected parse failure"
                            (isLeft (parseOnly p t))
expectParse p t a         = assertBool msg (r == a)
  where r   = parseOnly p t
        msg = Prelude.unwords
              ["Expected parse to", show a, ". Got", show r]

fromEither :: Either String a -> Assertion
fromEither (Left e)  = assertBool e  False
fromEither (Right _) = assertBool "" True
