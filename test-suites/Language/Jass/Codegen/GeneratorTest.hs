module Language.Jass.Codegen.GeneratorTest where

import Language.Jass.Codegen.Generator
import Language.Jass.Semantic.Check
import Language.Jass.Parser.Grammar
import LLVM.General.PrettyPrint

import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c

checkJassFile :: FilePath -> Assertion
checkJassFile path = do 
  res <- parseJassFile path
  case res of
    Left err -> assertFailure (show err)
    Right tree -> case checkModuleSemantic tree of
      Left err -> assertFailure (show err)
      Right context -> case uncurry3 generateLLVM context of
        Left err -> assertFailure (show err)
        Right llvmModule -> putStrLn $ showPretty llvmModule
          
simpleCodegenTest :: TestTree
simpleCodegenTest = testGroup "jass helloworld"
  [ --testCase "hello.j" $ checkJassFile "tests/hello.j"
  ]