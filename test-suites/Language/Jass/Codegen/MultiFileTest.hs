module Language.Jass.Codegen.MultiFileTest where

import Language.Jass.Program
import Language.Jass.JIT.Executing
import Language.Jass.JIT.Module
import Control.Monad.Trans.Except
import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

type NativeWriteln = CString -> IO ()
nativeWriteln :: NativeWriteln
nativeWriteln adr = putStrLn =<< fromJass adr
foreign import ccall "wrapper" mkNativeWriteln :: NativeWriteln -> IO (FunPtr NativeWriteln)

makeNativeTable :: NativeTableMaker
makeNativeTable _ = liftIO $ sequence [("writeln",) . castFunPtr <$> mkNativeWriteln nativeWriteln]

checkJassFolder :: FilePath -> NativeTableMaker -> Assertion
checkJassFolder folder natives = return ()

multifileTests :: TestTree
multifileTests = let dbgFlag = False in testGroup "jass helloworld"
  [ testCase "hello.j" $ checkJassFolder "tests/multifile" makeNativeTable
  ]