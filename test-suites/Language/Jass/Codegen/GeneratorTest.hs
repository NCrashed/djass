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

makeEmptyNativeTable :: IO [(String, FunPtr ())]
makeEmptyNativeTable = return []

checkJassFile :: FilePath -> IO [(String, FunPtr ())] -> (JITModule -> ExceptT String IO ()) -> Assertion
checkJassFile path nativeTable action = withContext $ \cntx -> do
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
        tbl <- liftIO nativeTable
        withJassJIT cntx tbl llvmModule $ \jit -> action jit
      case res2 of
        Left err -> assertFailure err
        Right _ -> return ()
  where
  putStrLn' = liftIO . putStrLn

checkHello :: JITModule -> ExceptT String IO ()
checkHello = executeMain
  
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

type GreaterFunc = CInt -> CInt -> IO CChar
foreign import ccall "dynamic"
  mkGreaterFunc :: FunPtr GreaterFunc -> GreaterFunc

type GreaterFuncIF = CInt -> CFloat -> IO CChar
foreign import ccall "dynamic"
  mkGreaterFuncIF :: FunPtr GreaterFuncIF -> GreaterFuncIF
  
type FibFunc = CInt -> IO CInt
foreign import ccall "dynamic"
  mkFibFunc :: FunPtr FibFunc -> FibFunc

type FactFunc = CInt -> IO CInt
foreign import ccall "dynamic"
  mkFactFunc :: FunPtr FactFunc -> FactFunc

type FactAndFibFunc = CInt -> IO CInt
foreign import ccall "dynamic"
  mkFactAndFibFunc :: FunPtr FactAndFibFunc -> FactAndFibFunc
              
checkMath :: JITModule -> ExceptT String IO ()
checkMath jit = do
  summ1 <- exec2 "summ" mkSummFunc 1 1
  liftIO $ summ1 @?= 2 
  summ2 <- exec2 "summ" mkSubsFunc 42 2
  liftIO $ summ2 @?= 44
  
  subs1 <- exec2 "subs" mkSubsFunc 42 2
  liftIO $ subs1 @?= 40
  subs2 <- exec2 "subs" mkSubsFunc 0 2
  liftIO $ subs2 @?= -2
  
  square1 <- exec1 "square" mkSquareFunc 2
  liftIO $ square1 @?= 4
  
  inc1 <- exec1 "inc" mkIncFunc 1
  liftIO $ inc1 @?= 2
  
  incf1 <- exec1 "incf" mkIncFloatFunc 1
  liftIO $ incf1 @?= 2.0
  
  gr1 <- exec2 "greater" mkSubsFunc 2 1
  liftIO $ gr1 @?= 1
  grif1 <- exec2 "greaterif" mkSubsFunc 0 2
  liftIO $ grif1 @?= 0
  
  fib1 <- exec1 "fib" mkFibFunc 2
  liftIO $ fib1 @?= 1
  fib2 <- exec1 "fib" mkFibFunc 11
  liftIO $ fib2 @?= 55
  
  fact1 <- exec1 "fact" mkFactFunc 2
  liftIO $ fact1 @?= 2
  fact2 <- exec1 "fact" mkFactFunc 11
  liftIO $ fact2 @?= 39916800
  
  factfib1 <- exec1 "factAndFib" mkFactAndFibFunc 2
  liftIO $ factfib1 @?= 3
  factfib2 <- exec1 "factAndFib" mkFactAndFibFunc 11
  liftIO $ factfib2 @?= 39916855
  where  
    exec1 = callFunc1 jit
    exec2 = callFunc2 jit

type GetGlobalIFunc = IO CInt
foreign import ccall "dynamic"
  mkGetGlobalIFunc :: FunPtr GetGlobalIFunc -> GetGlobalIFunc

type SetGlobalIFunc = CInt -> IO ()
foreign import ccall "dynamic"
  mkSetGlobalIFunc :: FunPtr SetGlobalIFunc -> SetGlobalIFunc

type GetGlobalJFunc = IO CString
foreign import ccall "dynamic"
  mkGetGlobalJFunc :: FunPtr GetGlobalJFunc -> GetGlobalJFunc
     
checkGlobal :: JITModule -> ExceptT String IO ()
checkGlobal jit = do
  globalI1 <- exec0 "getGlobalI" mkGetGlobalIFunc
  liftIO $ globalI1 @?= 42
  exec1 "setGlobalI" mkSetGlobalIFunc 23
  globalI2 <- exec0 "getGlobalI" mkGetGlobalIFunc
  liftIO $ globalI2 @?= 23
  globalJ2 <- fromJass =<< exec0 "getGlobalJ" mkGetGlobalJFunc
  liftIO $ globalJ2 @?= "Hello!"
  where
    exec0 = callFunc0 jit
    exec1 = callFunc1 jit
    
simpleCodegenTest :: TestTree
simpleCodegenTest = testGroup "jass helloworld"
  [ testCase "hello.j" $ checkJassFile "tests/hello.j" makeNativeTable checkHello,
    testCase "math.j" $ checkJassFile "tests/math.j" makeEmptyNativeTable checkMath,
    testCase "global.j" $ checkJassFile "tests/global.j" makeEmptyNativeTable checkGlobal
  ]