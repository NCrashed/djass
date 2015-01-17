module Language.Jass.Semantic.CheckTest where

import Language.Jass.Semantic.Check
import Language.Jass.Parser.Grammar

import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Control.Monad

checkJassFile :: FilePath -> Assertion
checkJassFile path = do 
  res <- parseJassFile path
  case res of
    Left err -> assertFailure (show err)
    Right tree -> case checkModuleSemantic tree of
      Left err -> assertFailure (show err)
      Right _ -> return ()

checkJassFiles :: [FilePath] -> Assertion
checkJassFiles paths = do
  reses <- mapM parseJassFile paths
  trees <- forM reses $ \res -> case res of
    Left err -> assertFailure (show err) >> return undefined
    Right tree -> return tree
  case checkModulesSemantic trees of
    Left err -> assertFailure (show err)
    Right _ -> return ()
          
commonSemanticTest :: TestTree
commonSemanticTest = testGroup "common.j and blizzard.j semantic analysis"
  [ testCase "common.j" $ checkJassFile "tests/common.j",
    testCase "blizzard.j" $ checkJassFiles ["tests/common.j", "tests/blizzard.j"]
  ]