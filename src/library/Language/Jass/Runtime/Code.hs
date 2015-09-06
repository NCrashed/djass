{-# LANGUAGE DeriveDataTypeable #-}
module Language.Jass.Runtime.Code(
    codeTypeStruct  
  , getCodeTypeDefs
  , generateCodeValue
  , getFreeCodeFuncName
  , getCodePtrFuncName
  , getCodeReturnTypeFuncName
  , getCodeArgsCountFuncName
  , getCodeArgFuncName
  , JassCodeRef
  , JassCode(..)
  , liftJassCode
  ) where
  
import Language.Jass.Codegen.Type
import Language.Jass.Codegen.Helpers
import Language.Jass.Codegen.Context
import Language.Jass.Runtime.Memory
import Language.Jass.JIT.Calling
import Language.Jass.JIT.Module
import Language.Jass.Semantic.Callable
import Language.Jass.JassType
import Language.Jass.Parser.AST.Parameter
import LLVM.General.AST as LLVM
import LLVM.General.AST.Type
import LLVM.General.AST.Global
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Except
import Foreign.Ptr
import Foreign.C.Types
import Data.Typeable

-- | Jass side
type JassCodeRef = Ptr ()
-- | Haskell side
data JassCode = JassCode {
  codeFunctionPtr :: FunPtr (),
  codeReturnType :: Maybe JassType,
  codeArgumentTypes :: [JassType]
} deriving Typeable

-- | Returns LLVM definitions to paste into runtime
getCodeTypeDefs :: [Definition]
getCodeTypeDefs = [getCodeTypeAlias, getCodePtrFunc, getCodeReturnTypeFunc, getCodeArgsCountFunc, getCodeArgFunc, freeCodePtr]

-- | Allocates new code value for callable
generateCodeValue :: Callable -> Codegen (Name, [Named Instruction])
generateCodeValue callable = do
  memName <- generateName "codemem"
  valName <- generateName "codeval"
  fptr <- generateName "fptr"
  fptrVal <- generateName "fptr_val"
  rtt <- generateName "rett"
  rtt_ <- generateName "rett_"
  argc <- generateName "argc"
  argc_ <- generateName "argc_"
  argvMem <- generateName "argv_mem"
  argvMem32 <- generateName "argv_mem_i32"
  argv <- generateName "argv"
  argv32 <- generateName "argv_i32"
  
  ftype <- getCallableLLVMType callable
  rtId <- getTypeId $ getCallableReturnType callable
  let params = getCallableParameters callable
  let argsCount = length params
  let argSetter i = do
        arrind <- generateName "arrind"
        arrind32 <- generateName "arrind32"
        argTypeId <- getTypeId $ Just $ getParamType (params !! i)
        return [
          arrind := getElementPtr (LocalReference (ptr i8) argvMem) [constInt 64 (4*i)],
          arrind32 := bitcast (LocalReference (ptr i8) arrind) (ptr i32),
          Do $ store (LocalReference (ptr i32) arrind32) (constInt 32 argTypeId)]
  argIdsInstructions <- mapM argSetter [0 .. argsCount - 1]     
  return (valName, [
    memName := globalCall (ptr i8) allocMemoryFuncName [constInt 32 codeTypeSize],
    valName := bitcast (LocalReference (ptr i8) memName) (ptr codeTypeStruct),
    -- setting pointer
    fptr := bitcast (LocalReference (ptr i8) memName) (ptr $ ptr i8),
    fptrVal := bitcast (globalFunc ftype (getCallableName callable)) (ptr i8),
    Do $ store (LocalReference (ptr $ ptr i8) fptr) (LocalReference (ptr i8) fptrVal),
    -- setting return type
    rtt := getElementPtr (LocalReference (ptr i8) memName) [constInt 64 pointerSize],
    rtt_ := bitcast (LocalReference (ptr i8) rtt) (ptr i32),
    Do $ store (LocalReference (ptr i32) rtt_) (constInt 32 rtId),
    -- setting args count
    argc := getElementPtr (LocalReference (ptr i8) memName) [constInt 64 (pointerSize + 4)],
    argc_ := bitcast (LocalReference (ptr i8) argc) (ptr i32),
    Do $ store (LocalReference (ptr i32) argc_) (constInt 32 argsCount),
    -- allocating argv
    argvMem := globalCall (ptr i8) allocMemoryFuncName [constInt 32 (4*argsCount)],
    argvMem32 := bitcast (LocalReference (ptr i8) argvMem) (ptr i32),
    argv := getElementPtr (LocalReference (ptr i8) memName) [constInt 64 $ pointerSize + 4 + 4],
    argv32 := bitcast (LocalReference (ptr i8) argv) (ptr $ ptr i32),
    Do $ store (LocalReference (ptr $ ptr i32) argv32) (LocalReference (ptr i32) argvMem32)]
    -- placing arguments ids
    ++ concat argIdsInstructions
    )

-- | Internal name of jass runtime function to free code reference
getFreeCodeFuncName :: String
getFreeCodeFuncName = "$__jass__freeCodePtr"

type FreeCodeFunc = JassCodeRef -> IO ()
foreign import ccall "dynamic"
  mkFreeCodeFunc :: FunPtr FreeCodeFunc -> FreeCodeFunc

freeCodePtr :: Definition
freeCodePtr = GlobalDefinition $ functionDefaults {
  returnType = VoidType,
  name = Name getFreeCodeFuncName,
  parameters = ([LLVM.Parameter (ptr codeTypeStruct) (Name "codePtr") []], False),
  basicBlocks = [
    BasicBlock (Name "entry") [
      Name "casted" := bitcast (LocalReference (ptr codeTypeStruct) (Name "codePtr")) (ptr i8),
      Do $ globalCall VoidType freeMemoryFuncName [LocalReference (ptr i8) $ Name "casted"]
    ]
    (Do retVoid)
  ]
}

getCodeTypeAlias :: Definition
getCodeTypeAlias = TypeDefinition (Name "code") (Just codeTypeStruct)

-- | Internal name of jass runtime function to get pointer from code value
getCodePtrFuncName :: String
getCodePtrFuncName = "$__jass__getCodePtr"

type GetCodePtrFunc = JassCodeRef -> IO (FunPtr ())
foreign import ccall "dynamic"
  mkGetCodePtrFunc :: FunPtr GetCodePtrFunc -> GetCodePtrFunc

getCodePtrFunc :: Definition
getCodePtrFunc = GlobalDefinition $ functionDefaults {
  returnType = ptr i8,
  name = Name getCodePtrFuncName,
  parameters = ([LLVM.Parameter (ptr codeTypeStruct) (Name "codePtr") []], False),
  basicBlocks = [
    BasicBlock (Name "entry") [
      Name "fp" := getElementPtr (LocalReference (ptr codeTypeStruct) $ Name "codePtr") [constInt 64 0, constInt 32 0],
      Name "res" := load (LocalReference (ptr $ ptr i8) $ Name "fp")
    ] (Do $ ret $ LocalReference (ptr i8) $ Name "res")
  ]
}

-- | Internal name of jass runtime function to get return type from code value
getCodeReturnTypeFuncName :: String
getCodeReturnTypeFuncName = "$__jass__getCodeReturnType"

type GetCodeReturnTypeFunc = JassCodeRef -> IO CInt
foreign import ccall "dynamic"
  mkGetCodeReturnTypeFunc :: FunPtr GetCodeReturnTypeFunc -> GetCodeReturnTypeFunc
  
getCodeReturnTypeFunc :: Definition
getCodeReturnTypeFunc = GlobalDefinition $ functionDefaults {
  returnType = i32,
  name = Name getCodeReturnTypeFuncName,
  parameters = ([LLVM.Parameter (ptr codeTypeStruct) (Name "codePtr") []], False),
  basicBlocks = [
    BasicBlock (Name "entry") [
      Name "fp" := getElementPtr (LocalReference (ptr codeTypeStruct) $ Name "codePtr") [constInt 64 0, constInt 32 1],
      Name "res" := load (LocalReference (ptr i32) $ Name "fp")
    ] (Do $ ret $ LocalReference i32 $ Name "res")
  ]
}

-- | Internal name of jass runtime function to get args count from code value
getCodeArgsCountFuncName :: String
getCodeArgsCountFuncName = "$__jass__getCodeArgsCount"

type GetCodeArgsCountTypeFunc = JassCodeRef -> IO CInt
foreign import ccall "dynamic"
  mkGetCodeArgsCountTypeFunc :: FunPtr GetCodeArgsCountTypeFunc -> GetCodeArgsCountTypeFunc
  
getCodeArgsCountFunc :: Definition
getCodeArgsCountFunc = GlobalDefinition $ functionDefaults {
  returnType = i32,
  name = Name getCodeArgsCountFuncName,
  parameters = ([LLVM.Parameter (ptr codeTypeStruct) (Name "codePtr") []], False),
  basicBlocks = [
    BasicBlock (Name "entry") [
      Name "fp" := getElementPtr (LocalReference (ptr codeTypeStruct) $ Name "codePtr") [constInt 64 0, constInt 32 2],
      Name "res" := load (LocalReference (ptr i32) $ Name "fp")
    ] (Do $ ret $ LocalReference i32 $ Name "res")
  ]
}

-- | Internal name of jass runtime function to get specified argument from code value
getCodeArgFuncName :: String
getCodeArgFuncName = "$__jass__getCodeArg"

type GetCodeArgTypeFunc = JassCodeRef -> CInt -> IO CInt
foreign import ccall "dynamic"
  mkGetCodeArgTypeFunc :: FunPtr GetCodeArgTypeFunc -> GetCodeArgTypeFunc
  
getCodeArgFunc :: Definition
getCodeArgFunc = GlobalDefinition $ functionDefaults {
  returnType = i32,
  name = Name getCodeArgFuncName,
  parameters = ([LLVM.Parameter (ptr codeTypeStruct) (Name "codePtr") [],
                 LLVM.Parameter i32 (Name "index") []], False),
  basicBlocks = [
    BasicBlock (Name "entry") [
      Name "indexext" := SExt (LocalReference i32 (Name "index")) i64 [],
      Name "args" := getElementPtr (LocalReference (ptr codeTypeStruct) $ Name "codePtr") [constInt 64 0, constInt 32 3],
      Name "arrptr" := load (LocalReference (ptr $ ptr i32) $ Name "args"),
      Name "ptr" := getElementPtr (LocalReference (ptr i32) $ Name "arrptr") [LocalReference i64 $ Name "indexext"],
      Name "res" := load (LocalReference (ptr i32) $ Name "ptr") 
    ] (Do $ ret $ LocalReference i32 $ Name "res")
  ]
}

-- | Reading full info about jass code reference
liftJassCode :: JITModule -- ^ Loaded jass module the code value came from 
  -> JassCodeRef -- ^ code reference from jass module (e.x. passed by native or returned by func)  
  -> ExceptT String IO JassCode -- ^ Loaded jass code value, ready for executing
liftJassCode jit@(JITModule (tm, _) _) jcode = do
  fptr <- callFunc1 jit getCodePtrFuncName mkGetCodePtrFunc jcode
  retid <- callFunc1 jit getCodeReturnTypeFuncName mkGetCodeReturnTypeFunc jcode
  argc <- callFunc1 jit getCodeArgsCountFuncName mkGetCodeArgsCountTypeFunc jcode
  argIds <- mapM (callFunc2 jit getCodeArgFuncName mkGetCodeArgTypeFunc jcode) [0 .. argc-1]
  retType <- if retid == 0 then return Nothing else Just <$> lookup' (fromIntegral retid) tm
  argsTypes <- mapM (`lookup'` tm) (fmap fromIntegral argIds)
  callFunc1 jit getFreeCodeFuncName mkFreeCodeFunc jcode
  return JassCode {
    codeFunctionPtr = fptr,
    codeReturnType = retType,
    codeArgumentTypes = argsTypes
  }
  where
  lookup' s m = case HM.lookup s m of
    Nothing -> throwE $ "Cannot find type with id " ++ show s ++ ", got malformed code reference!"
    Just jt -> return jt