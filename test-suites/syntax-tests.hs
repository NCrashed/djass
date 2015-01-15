module Main where
import Language.Jass.Parser.GrammarTest
import Language.Jass.Parser.ASTTest


import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [syntaxTests]