module Language.Jass.Codegen.GeneratorTest where

import Language.Jass.JIT.Module
import LLVM.General.Module
import LLVM.General.Context

import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Control.Monad.Trans.Except

import Foreign.C.String

nativeWriteln :: CString -> IO ()
nativeWriteln adr = putStrLn =<< peekCString adr

foreign export ccall "writeln" nativeWriteln :: CString -> IO ()

  
checkJassFile :: FilePath -> Assertion
checkJassFile path = withContext $ \cntx -> do 
  res <- runExceptT $ loadJassModuleFromFile path
  case res of
    Left err -> assertFailure err
    Right astModule -> do
      res2 <- runExceptT $ withModuleFromAST cntx astModule $ \llvmModule -> do
        optimizeModule llvmModule
        source <- moduleLLVMAssembly llvmModule
        executeMain cntx llvmModule
        return source
      case res2 of
        Left err -> assertFailure err
        Right r -> putStrLn r
 
          
simpleCodegenTest :: TestTree
simpleCodegenTest = testGroup "jass helloworld"
  [ testCase "hello.j" $ checkJassFile "tests/hello.j"
  ]