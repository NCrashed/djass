module Language.Jass.Runtime.Memory(
    allocMemoryFuncName
  , getAllocMemoryDefs
  , AllocMemoryFunc
  , setDefaultAllocator
  , setAllocator
  ) where

import Language.Jass.JIT.Module
import Language.Jass.JIT.Calling
import LLVM.General.AST as LLVM
import LLVM.General.AST.Type as LLVM
import LLVM.General.AST.Global as Glob
import LLVM.General.AST.Constant as Const
import LLVM.General.AST.Visibility
import LLVM.General.AST.Linkage
import LLVM.General.AST.CallingConvention
import Control.Monad.Trans.Except
import Control.Monad.Trans (liftIO)
import Control.Applicative
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

allocMemoryFuncName :: String
allocMemoryFuncName = "$__jass__allocMemory"

allocMemoryFuncSetterName :: String
allocMemoryFuncSetterName = "$__jass__SetAllocMemory"

getAllocMemoryDefs :: [LLVM.Definition]
getAllocMemoryDefs = [globalVar, varSetter, varCaller]
  where
  globaVarName = "$__jass__allocMemoryVar"
  funcType = FunctionType {
    resultType = ptr i8,
    argumentTypes = [i32],
    isVarArg = False
  }
  
  globalVar = GlobalDefinition $ globalVariableDefaults {
    name = Name globaVarName,
    linkage = Private,
    visibility = Hidden,
    Glob.type' = ptr funcType,
    initializer = Just $ Null $ ptr funcType
  }
  
  varSetter = GlobalDefinition $ functionDefaults {
    returnType = VoidType,
    name = Name allocMemoryFuncSetterName,
    parameters = ([LLVM.Parameter (ptr funcType) (Name "ptr") []], False),
    basicBlocks = [
      BasicBlock (Name "entry") [
        Do $ Store False 
          (ConstantOperand $ GlobalReference (ptr $ ptr funcType) $ Name globaVarName)
          (LocalReference (ptr funcType) $ Name "ptr") Nothing 0 []
      ] (Do $ Ret Nothing [])
    ]
  }
  
  varCaller = GlobalDefinition $ functionDefaults {
    returnType = ptr i8,
    name = Name allocMemoryFuncName,
    linkage = Private,
    visibility = Hidden,
    parameters = ([LLVM.Parameter i32 (Name "size") []], False),
    basicBlocks = [
      BasicBlock (Name "entry") [
        Name "fptr" := Load False (ConstantOperand $ GlobalReference (ptr $ ptr funcType) (Name globaVarName)) Nothing 0 [],
        Name "res" := Call False C [] (Right $ LocalReference (ptr funcType) $ Name "fptr") [(LocalReference i32 $ Name "size", [])] [] []
      ] (Do $ Ret (Just $ LocalReference (ptr i8) $ Name "res") [])
    ]
  }
  
type AllocMemoryFunc = CInt -> IO (Ptr CChar)
foreign import ccall "wrapper"
  mkAllocMemoryFunc :: AllocMemoryFunc -> IO (FunPtr AllocMemoryFunc)

type AllocMemorySetter = FunPtr () -> IO ()
foreign import ccall "dynamic"
  mkAllocMemorySetter :: FunPtr AllocMemorySetter -> AllocMemorySetter
  
defaultAllocator :: AllocMemoryFunc
defaultAllocator i = mallocArray $ fromIntegral i

setAllocator :: JITModule -> AllocMemoryFunc -> ExceptT String IO ()
setAllocator jit func = callFunc1 jit allocMemoryFuncSetterName mkAllocMemorySetter =<< liftIO (castFunPtr <$> mkAllocMemoryFunc func)

setDefaultAllocator :: JITModule -> ExceptT String IO ()
setDefaultAllocator jit = setAllocator jit defaultAllocator