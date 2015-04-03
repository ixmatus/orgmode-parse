{-# LANGUAGE OverloadedStrings #-}

module Document where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Monoid
import Test.Tasty
import Test.Tasty.HUnit

import Data.OrgMode.Parse.Types
import Data.OrgMode.Parse.Attoparsec.Document
import Util

parserSmallDocumentTests :: TestTree
parserSmallDocumentTests = testGroup "Attoparsec Small Document"
  [ (testCase "Parse Empty Document" $ testDocS "" (Document mempty mempty))
  , (testCase "Parse No Headlines"   $ testDocS pText (Document pText []))
  ]
  where testDocS s r = expectParse (parseDocument kw) s (Right r)
        testDocF s   = expectParse (parseDocument kw) s (Left "Some failure")
        kw           = ["TODO", "CANCELED", "DONE"]
        pText        = "Paragraph text\n.No headline here.\n##--------\n"
