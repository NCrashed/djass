{-# LANGUAGE TupleSections, TypeOperators #-}
module Language.Jass.Codegen.GeneratorTest where

import Language.Jass.Runtime.Code
import Language.Jass.JIT.Executing
import Language.Jass.JIT.Calling
import Language.Jass.JIT.Module
import Language.Jass.JassType
import LLVM.General.Context
import LLVM.General.PrettyPrint
import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Applicative

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Data.Maybe

type NativeWriteln = CString -> IO ()
nativeWriteln :: NativeWriteln
nativeWriteln adr = putStrLn =<< fromJass adr
foreign import ccall "wrapper" mkNativeWriteln :: NativeWriteln -> IO (FunPtr NativeWriteln)

makeNativeTable :: JITModule -> ExceptT String IO [(String, FunPtr ())]
makeNativeTable _ = liftIO $ sequence [("writeln",) . castFunPtr <$> mkNativeWriteln nativeWriteln]

makeEmptyNativeTable :: JITModule -> ExceptT String IO [(String, FunPtr ())]
makeEmptyNativeTable _ = return []

checkJassFile :: FilePath -> Bool -> (JITModule -> ExceptT String IO [(String, FunPtr ())]) -> (JITModule -> ExceptT String IO ()) -> Assertion
checkJassFile path dbgFlag nativeTable action = withContext $ \cntx -> do
  res <- runExceptT $ loadJassModuleFromFile path
  case res of
    Left err -> assertFailure err
    Right astModule -> do
      when dbgFlag $ putStrLn =<< showPretty <$> extractAST astModule
      res2 <- runExceptT $ withRaisedAST cntx astModule $ \llvmModule -> do
        when dbgFlag $ putStrLn' =<<  moduleAssembly llvmModule
        optimizeModule llvmModule
        when dbgFlag $ putStrLn' "Optimized: "
        when dbgFlag $ putStrLn' =<< moduleAssembly llvmModule
        withJassJIT cntx nativeTable llvmModule action
      case res2 of
        Left err -> assertFailure err
        Right _ -> return ()
  where
  putStrLn' = liftIO . putStrLn

checkHello :: JITModule -> ExceptT String IO ()
checkHello = callMain
  
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

type TestStrings = CString -> IO CString
foreign import ccall "dynamic"
  mkTestStrings :: FunPtr TestStrings -> TestStrings
     
checkString :: JITModule -> ExceptT String IO ()
checkString jit = do
  res1 <-fromJass =<< exec1 "testStrings" mkTestStrings =<< toJass "Hello, "
  liftIO $ res1 @?= "Hello, World!"
  res2 <-fromJass =<< exec1 "testStrings" mkTestStrings =<< toJass "Bye, "
  liftIO $ res2 @?= "Bye, Dude!"
  res3 <-fromJass =<< exec1 "testStrings" mkTestStrings =<< toJass "Something"
  liftIO $ res3 @?= "I don't know that input!"
  where
    exec1 = callFunc1 jit

type SetA = CInt -> CFloat -> IO ()
foreign import ccall "dynamic"
  mkSetA :: FunPtr SetA -> SetA

type GetA = CInt -> IO CFloat
foreign import ccall "dynamic"
  mkGetA :: FunPtr GetA -> GetA

type TestLocalArray = CInt -> IO CInt
foreign import ccall "dynamic"
  mkTestLocalArray :: FunPtr TestLocalArray -> TestLocalArray
    
checkArray :: JITModule -> ExceptT String IO ()
checkArray jit = do
  exec2 "setA" mkSetA 23 0.42
  res1 <- exec1 "getA" mkGetA 23
  liftIO $ res1 @?= 0.42
  
  res2 <- exec1 "testLocalArray" mkTestLocalArray 100
  liftIO $ res2 @?= sum [0 .. 99]
  where
    exec1 = callFunc1 jit
    exec2 = callFunc2 jit

type MyCallback = CInt -> CFloat -> IO ()
foreign import ccall "dynamic"
  mkMyCallback :: FunPtr MyCallback -> MyCallback

type MyGetA = IO CInt
foreign import ccall "dynamic"
  mkMyGetA :: FunPtr MyGetA -> MyGetA
  
type MyGetB = IO CFloat
foreign import ccall "dynamic"
  mkMyGetB :: FunPtr MyGetB -> MyGetB
    
type NativeSetCallback = JassCodeRef -> IO ()
nativeSetCallback :: JITModule -> NativeSetCallback
nativeSetCallback jit adr = do
  mjcode <- runExceptT $ liftJassCode jit adr
  case mjcode of
    Left msg -> putStrLn msg
    Right jcode -> do
      putStrLn $ "Code return type " ++ show (codeReturnType jcode)
      assertBool "Code return type is invalid" $ isNothing (codeReturnType jcode) 
      putStrLn $ "Code args types " ++ show (codeArgumentTypes jcode)
      assertBool "Code args are invalid" $ codeArgumentTypes jcode == [JInteger, JReal]
      mkMyCallback (castFunPtr $ codeFunctionPtr jcode) 23 42.0
      
foreign import ccall "wrapper" mkNativeSetCallback :: NativeSetCallback -> IO (FunPtr NativeSetCallback)

makeCheckCodeNativeTable :: JITModule -> ExceptT String IO [(String, FunPtr ())]
makeCheckCodeNativeTable jit = liftIO $ sequence [("setCallback",) . castFunPtr <$> mkNativeSetCallback (nativeSetCallback jit)]

checkCode :: JITModule -> ExceptT String IO ()
checkCode jit = do
  callMain jit
  resA <- exec0 "getA" mkMyGetA
  liftIO $ resA @?= 23 
  resB <- exec0 "getB" mkMyGetB
  liftIO $ resB @?= 42.0  
  where
    exec0 = callFunc0 jit
         
simpleCodegenTest :: TestTree
simpleCodegenTest = let dbgFlag = False in testGroup "jass helloworld"
  [ testCase "hello.j" $ checkJassFile "tests/hello.j" dbgFlag makeNativeTable checkHello,
    testCase "math.j" $ checkJassFile "tests/math.j" dbgFlag makeEmptyNativeTable checkMath,
    testCase "global.j" $ checkJassFile "tests/global.j" dbgFlag makeEmptyNativeTable checkGlobal,
    testCase "string.j" $ checkJassFile "tests/string.j" dbgFlag makeEmptyNativeTable checkString,
    testCase "array.j" $ checkJassFile "tests/array.j" dbgFlag makeEmptyNativeTable checkArray,
    testCase "code.j" $ checkJassFile "tests/code.j" dbgFlag makeCheckCodeNativeTable checkCode
  ]