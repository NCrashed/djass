{-# LANGUAGE TupleSections #-}
module Language.Jass.Codegen.MultiFileTest where

import Language.Jass.Program
import Language.Jass.TestUtils
import Control.Monad.IO.Class

import Test.Tasty
import Test.Tasty.HUnit

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

type NativeWriteln = CString -> IO ()
nativeWriteln :: NativeWriteln
nativeWriteln adr = putStrLn =<< fromJass adr
foreign import ccall "wrapper" mkNativeWriteln :: NativeWriteln -> IO (FunPtr NativeWriteln)

type NativeI2S = CInt -> IO CString
nativeI2S :: NativeI2S
nativeI2S i = toJass.show =<< fromJass i
foreign import ccall "wrapper" mkNativeI2S :: NativeI2S -> IO (FunPtr NativeI2S)

makeNativeTable :: NativeTableMaker
makeNativeTable _ = liftIO $ sequence [ ("writeln",) . castFunPtr <$> mkNativeWriteln nativeWriteln
                                      , ("I2S",) . castFunPtr <$> mkNativeI2S nativeI2S ]

checkJassFolder :: FilePath -> NativeTableMaker -> Assertion
checkJassFolder folder natives = assertExcept $ do
  prog <- constructProgramFromSource folder "main.j"
  executeProgram prog natives callMain

multifileTests :: TestTree
multifileTests = testGroup "multifile helloworld"
  [ testCase "hello.j" $ checkJassFolder "tests/multifile" makeNativeTable
  ]