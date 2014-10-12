{-# LANGUAGE RankNTypes #-}

module Main where

import           Data.Text                          as T

import           Data.Attoparsec.Text
import           Data.StructMsg.Attoparsec.Headings
import           Data.StructMsg.Internal
import           Test.HUnit
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ parserHeaderTests ]

parserHeaderTests :: TestTree
parserHeaderTests = testGroup "Parser Header Tests"
    [ (testCase "Parse Header" $ testHeader Thermometer "thermometer")
    , (testCase "Parse Header" $ testHeader Humidity "humidity")
    ]

testHeader :: Sensor -> String -> Assertion
testHeader e v = (Right e) @=? (parseOnly sensorParser $ T.pack v)
