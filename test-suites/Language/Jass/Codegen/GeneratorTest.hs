{-# LANGUAGE TupleSections #-}
module Language.Jass.Codegen.GeneratorTest where

import Language.Jass.JIT.Module
import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.PrettyPrint
import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Applicative

import Foreign.C.String
import Foreign.Ptr

type NativeWriteln = CString -> IO ()
nativeWriteln :: NativeWriteln
nativeWriteln adr = putStrLn =<< peekCString adr
foreign import ccall "wrapper" mkNativeWriteln :: NativeWriteln -> IO (FunPtr NativeWriteln)

makeNativeTable :: IO [(String, FunPtr ())]
makeNativeTable = sequence [("writeln",) . castFunPtr <$> mkNativeWriteln nativeWriteln]
  
--testAsm = "@global = global void (i8*)* null" ++ "\n"
--  ++ "define void @main(void (i8*)* %ptr) {" ++ "\n"
--  ++ "entry:" ++ "\n"
--  ++ "store void (i8*)* %ptr, void (i8*)** @global" ++ "\n"
--  ++ "ret void" ++ "\n"
--  ++ "}" ++ "\n"
--
--foo cntx = do
--  withModuleFromLLVMAssembly cntx testAsm $ \llvmModule -> do
--    astModule <- moduleAST llvmModule
--    putStrLn $ showPretty astModule
     
checkJassFile :: FilePath -> Assertion
checkJassFile path = withContext $ \cntx -> do 
  --putStrLn =<< show <$> runExceptT (foo cntx)
  res <- runExceptT $ loadJassModuleFromFile path
  case res of
    Left err -> assertFailure err
    Right astModule -> do
      putStrLn =<< showPretty <$> extractAST astModule
      res2 <- runExceptT $ withRaisedAST cntx astModule $ \llvmModule -> do
        putStrLn' =<<  moduleAssembly llvmModule
        optimizeModule llvmModule
        putStrLn' "Optimized: "
        putStrLn' =<< moduleAssembly llvmModule
        table <- liftIO makeNativeTable
        executeMain cntx table llvmModule
      case res2 of
        Left err -> assertFailure err
        Right _ -> return ()
  where
  putStrLn' = liftIO . putStrLn
  
simpleCodegenTest :: TestTree
simpleCodegenTest = testGroup "jass helloworld"
  [ testCase "hello.j" $ checkJassFile "tests/hello.j"
  ]