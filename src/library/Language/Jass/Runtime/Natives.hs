module Language.Jass.Runtime.Natives(
  callNativeBinder
  ) where

import Language.Jass.JIT.Calling
import Language.Jass.JIT.Module
import LLVM.General.AST as LLVMAST
import Control.Monad.Trans.Except
import Foreign.Ptr

type NativeBinder = FunPtr () -> IO ()
foreign import ccall "dynamic"
  mkNativeBinder :: FunPtr NativeBinder -> NativeBinder
  
callNativeBinder :: JITModule -> LLVMAST.Name -> FunPtr () -> ExceptT String IO ()
callNativeBinder ex (LLVMAST.Name nativeName) = callFunc1 ex nativeName mkNativeBinder
callNativeBinder ex (LLVMAST.UnName i) = callFunc1 ex (show i) mkNativeBinder