{-# LANGUAGE TupleSections, TypeOperators #-}
module Language.Jass.Codegen.GeneratorTest where

import Language.Jass.JIT.Module
import Language.Jass.JIT.Calling
import LLVM.General.Context
import LLVM.General.PrettyPrint
import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Applicative

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

type NativeWriteln = CString -> IO ()
nativeWriteln :: NativeWriteln
nativeWriteln adr = putStrLn =<< fromJass adr
foreign import ccall "wrapper" mkNativeWriteln :: NativeWriteln -> IO (FunPtr NativeWriteln)

makeNativeTable :: IO [(String, FunPtr ())]
makeNativeTable = sequence [("writeln",) . castFunPtr <$> mkNativeWriteln nativeWriteln]
     
checkJassFile :: FilePath -> (Context -> UnlinkedModule -> ExceptT String IO ()) -> Assertion
checkJassFile path action = withContext $ \cntx -> do
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
        action cntx llvmModule
      case res2 of
        Left err -> assertFailure err
        Right _ -> return ()
  where
  putStrLn' = liftIO . putStrLn

checkHello :: Context -> UnlinkedModule -> ExceptT String IO ()
checkHello cntx llvmModule = do
  table <- liftIO makeNativeTable
  executeMain cntx table llvmModule
  
type SummFunc = CInt -> CInt -> IO CInt
foreign import ccall "dynamic"
  mkSummFunc :: FunPtr SummFunc -> SummFunc
  
type SubsFunc = CInt -> CInt -> IO CInt
foreign import ccall "dynamic"
  mkSubsFunc :: FunPtr SubsFunc -> SubsFunc
  
type SquareFunc = CInt -> IO CInt
foreign import ccall "dynamic"
  mkSquareFunc :: FunPtr SquareFunc -> SquareFunc

type IncFunc = CInt -> IO CInt
foreign import ccall "dynamic"
  mkIncFunc :: FunPtr IncFunc -> IncFunc

type IncFloatFunc = CFloat -> IO CFloat
foreign import ccall "dynamic"
  mkIncFloatFunc :: FunPtr IncFloatFunc -> IncFloatFunc
         
checkMath :: Context -> UnlinkedModule -> ExceptT String IO ()
checkMath cntx llvmModule = do
  summ1 <- executeJass2 cntx [] llvmModule "summ" mkSummFunc 1 1
  liftIO $ summ1 @?= 2 
  summ2 <- executeJass2 cntx [] llvmModule "summ" mkSubsFunc 42 2
  liftIO $ summ2 @?= 44
  
  subs1 <- executeJass2 cntx [] llvmModule "subs" mkSubsFunc 42 2
  liftIO $ subs1 @?= 40
  subs2 <- executeJass2 cntx [] llvmModule "subs" mkSubsFunc 0 2
  liftIO $ subs2 @?= -2
  
  square1 <- executeJass1 cntx [] llvmModule "square" mkSquareFunc 2
  liftIO $ square1 @?= 4
  
  inc1 <- executeJass1 cntx [] llvmModule "inc" mkIncFunc 1
  liftIO $ inc1 @?= 2
  
  incf1 <- executeJass1 cntx [] llvmModule "incf" mkIncFloatFunc 1
  liftIO $ incf1 @?= 2.0
  
simpleCodegenTest :: TestTree
simpleCodegenTest = testGroup "jass helloworld"
  [ testCase "hello.j" $ checkJassFile "tests/hello.j" checkHello,
    testCase "math.j" $ checkJassFile "tests/math.j" checkMath
  ]