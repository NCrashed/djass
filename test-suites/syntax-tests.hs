module Main where

import Language.Jass.Parser.GrammarTest
import Language.Jass.Semantic.CheckTest


import Test.Tasty
--import Test.Tasty.QuickCheck as QC
--import Test.Tasty.HUnit

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  testGroup "Parsing" [syntaxTests, commonParsing],
  testGroup "Semantic checks" [commonSemanticTest]
  ]