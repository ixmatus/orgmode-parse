module Util where

import           Data.Attoparsec.Text
import           Data.Attoparsec.Types as TP
import           Data.Text             as T
import           Test.HUnit

testParser :: (TP.Parser Text a) -> String -> Assertion
testParser f v = fromEither (parseOnly f $ T.pack v)

fromEither :: Either String a -> Assertion
fromEither (Left e)  = assertBool e  False
fromEither (Right _) = assertBool "" True
