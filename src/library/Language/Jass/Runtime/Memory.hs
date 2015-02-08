module Language.Jass.Runtime.Memory(
    allocMemoryFuncName
  , freeMemoryFuncName
  , memorySetFuncName
  , getMemoryDefs
  , AllocMemoryFunc
  , FreeMemoryFunc
  , MemorySetFunc
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
import Control.Monad as Monad
import Control.Applicative
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Data.Word

allocMemoryFuncName :: String
allocMemoryFuncName = "$__jass__allocMemory"

allocMemoryFuncSetterName :: String
allocMemoryFuncSetterName = "$__jass__SetAllocMemory"

freeMemoryFuncName :: String
freeMemoryFuncName = "$__jass__freeMemory"

freeMemoryFuncSetterName :: String
freeMemoryFuncSetterName = "$__jass__SetFreeMemory"

memorySetFuncSetterName :: String
memorySetFuncSetterName = "$__jass__SetMemorySet"

memorySetFuncName :: String
memorySetFuncName = "$__jass__memset"

getMemoryDefs :: [LLVM.Definition]
getMemoryDefs = getAllocMemoryDefs ++ getFreeMemoryDefs ++ getMemorySetDefs

getAllocMemoryDefs :: [LLVM.Definition]
getAllocMemoryDefs = genRuntimeDefs "$__jass__allocMemoryVar" allocMemoryFuncSetterName allocMemoryFuncName funcType pars
  where
  funcType = FunctionType {
    resultType = ptr i8,
    argumentTypes = [i32],
    isVarArg = False
  }
  pars = [LLVM.Parameter i32 (Name "size") []]
  
getFreeMemoryDefs :: [LLVM.Definition]
getFreeMemoryDefs = genRuntimeDefs "$__jass__freeMemoryVar" freeMemoryFuncSetterName freeMemoryFuncName funcType pars
  where
  funcType = FunctionType {
    resultType = VoidType,
    argumentTypes = [ptr i8],
    isVarArg = False
  }
  pars = [LLVM.Parameter (ptr i8) (Name "ptr") []]
  
getMemorySetDefs :: [LLVM.Definition]
getMemorySetDefs = genRuntimeDefs "$__jass__memorySetVar" memorySetFuncSetterName memorySetFuncName funcType pars
  where
  funcType = FunctionType {
    resultType = VoidType,
    argumentTypes = [ptr i8, i8, i32],
    isVarArg = False
  }
  pars = [LLVM.Parameter (ptr i8) (Name "ptr") [], LLVM.Parameter i8 (Name "value") [], LLVM.Parameter i32 (Name "size") []]
  
genRuntimeDefs :: String -> String -> String -> LLVM.Type -> [LLVM.Parameter] -> [LLVM.Definition]
genRuntimeDefs globalVarName setterName funcName funcType@(FunctionType resType _ _) pars = [globalVar, varSetter, varCaller]
  where
  args = fmap (\(LLVM.Parameter t n _) -> LocalReference t n) pars `zip` repeat [] 
  globalVar = GlobalDefinition $ globalVariableDefaults {
    name = Name globalVarName,
    linkage = Private,
    visibility = Hidden,
    Glob.type' = ptr funcType,
    initializer = Just $ Null $ ptr funcType
  }
  
  varSetter = GlobalDefinition $ functionDefaults {
    returnType = VoidType,
    name = Name setterName,
    parameters = ([LLVM.Parameter (ptr funcType) (Name "ptr") []], False),
    basicBlocks = [
      BasicBlock (Name "entry") [
        Do $ Store False 
          (ConstantOperand $ GlobalReference (ptr $ ptr funcType) $ Name globalVarName)
          (LocalReference (ptr funcType) $ Name "ptr") Nothing 0 []
      ] (Do $ Ret Nothing [])
    ]
  }
  
  varCaller = GlobalDefinition $ functionDefaults {
    returnType = resType,
    name = Name funcName,
    linkage = Private,
    visibility = Hidden,
    parameters = (pars, False),
    basicBlocks = if resType == VoidType then [
      BasicBlock (Name "entry") [
        Name "fptr" := Load False (ConstantOperand $ GlobalReference (ptr $ ptr funcType) (Name globalVarName)) Nothing 0 [],
        Do $ Call False C [] (Right $ LocalReference (ptr funcType) $ Name "fptr") args [] []
      ] (Do $ Ret Nothing [])
    ] else [
      BasicBlock (Name "entry") [
        Name "fptr" := Load False (ConstantOperand $ GlobalReference (ptr $ ptr funcType) (Name globalVarName)) Nothing 0 [],
        Name "res" := Call False C [] (Right $ LocalReference (ptr funcType) $ Name "fptr") args [] []
      ] (Do $ Ret (Just $ LocalReference resType $ Name "res") [])
    ]
   
  }
genRuntimeDefs _ _ _ _ _ = error "Invalid type of function at runtime generation helper"
   
type AllocMemoryFunc = CInt -> IO (Ptr CChar)
foreign import ccall "wrapper"
  mkAllocMemoryFunc :: AllocMemoryFunc -> IO (FunPtr AllocMemoryFunc)

type AllocMemorySetter = FunPtr () -> IO ()
foreign import ccall "dynamic"
  mkAllocMemorySetter :: FunPtr AllocMemorySetter -> AllocMemorySetter

type FreeMemoryFunc = Ptr () -> IO ()
foreign import ccall "wrapper"
  mkFreeMemoryFunc :: FreeMemoryFunc -> IO (FunPtr FreeMemoryFunc)

type FreeMemorySetter = FunPtr () -> IO ()
foreign import ccall "dynamic"
  mkFreeMemorySetter :: FunPtr FreeMemorySetter -> FreeMemorySetter

type MemorySetFunc = Ptr () -> CInt -> CInt -> IO ()
foreign import ccall "wrapper"
  mkMemorySetFunc :: MemorySetFunc -> IO (FunPtr MemorySetFunc)

type MemorySetSetter = FunPtr () -> IO ()
foreign import ccall "dynamic"
  mkMemorySetSetter :: FunPtr MemorySetSetter -> MemorySetSetter
    
defaultAllocator :: AllocMemoryFunc
defaultAllocator i = mallocArray $ fromIntegral i

defaultFree :: FreeMemoryFunc
defaultFree = free

foreign import ccall unsafe "string.h memset" c_memset
  :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)
    
defaultMemset :: MemorySetFunc
defaultMemset mptr val = Monad.void . c_memset (castPtr mptr) val . fromIntegral

setAllocator :: JITModule -> AllocMemoryFunc -> FreeMemoryFunc -> MemorySetFunc -> ExceptT String IO ()
setAllocator jit allocFunc freeFunc memsetFunc = do
  callFunc1 jit allocMemoryFuncSetterName mkAllocMemorySetter =<< liftIO (castFunPtr <$> mkAllocMemoryFunc allocFunc)
  callFunc1 jit freeMemoryFuncSetterName mkFreeMemorySetter =<< liftIO (castFunPtr <$> mkFreeMemoryFunc freeFunc)
  callFunc1 jit memorySetFuncSetterName mkMemorySetSetter =<< liftIO (castFunPtr <$> mkMemorySetFunc memsetFunc)

setDefaultAllocator :: JITModule -> ExceptT String IO ()
setDefaultAllocator jit = setAllocator jit defaultAllocator defaultFree defaultMemset