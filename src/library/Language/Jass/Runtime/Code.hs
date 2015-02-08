module Language.Jass.Runtime.Code(
    codeTypeStruct  
  , getCodeTypeDefs
  , getCodePtrFuncName
  , getCodeReturnTypeFuncName
  , getCodeArgsCountFuncName
  , getCodeArgFuncName
  ) where
  
import Language.Jass.Codegen.Type
import Language.Jass.Codegen.Helpers
import LLVM.General.AST as LLVM
import LLVM.General.AST.Type
import LLVM.General.AST.Global

codeTypeStruct :: Type
codeTypeStruct = StructureType {
    isPacked = False,
    elementTypes = [
        ptr i8  -- function pointer
      , i32     -- id of return type, 0 for nothing
      , i32     -- count of arguments
      , ptr i32 -- array of arguments types as ids
    ]
  }

getCodeTypeDefs :: [Definition]
getCodeTypeDefs = [getCodeTypeAlias, getCodePtrFunc, getCodeReturnTypeFunc, getCodeArgsCountFunc, getCodeArgFunc]

getCodeTypeAlias :: Definition
getCodeTypeAlias = TypeDefinition (Name "code") (Just codeTypeStruct)

getCodePtrFuncName :: String
getCodePtrFuncName = "$__jass__getCodePtr"

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

getCodeReturnTypeFuncName :: String
getCodeReturnTypeFuncName = "$__jass__getCodeReturnType"

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

getCodeArgsCountFuncName :: String
getCodeArgsCountFuncName = "$__jass__getCodeArgsCount"

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

getCodeArgFuncName :: String
getCodeArgFuncName = "$__jass__getCodeArg"

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