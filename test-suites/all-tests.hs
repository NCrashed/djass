module Main where

import Language.Jass.Parser.GrammarTest
import Language.Jass.Semantic.CheckTest
import Language.Jass.Codegen.GeneratorTest
import Language.Jass.Codegen.MultiFileTest

import Test.Tasty
--import Test.Tasty.QuickCheck as QC
--import Test.Tasty.HUnit

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
  testGroup "Parsing" [syntaxTests, simpleParsing, commonParsing],
  testGroup "Semantic checks" [commonSemanticTest],
  testGroup "Code generation" [simpleCodegenTest],
  testGroup "Integration tests" [multifileTests]
  ]