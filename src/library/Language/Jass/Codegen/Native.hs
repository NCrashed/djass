module Language.Jass.Codegen.Native(
    generateNativeSupport
  ) where
  
import Language.Jass.Codegen.Context
import Language.Jass.Codegen.Type
import Language.Jass.Parser.AST as AST
import LLVM.General.AST as LLVM
import LLVM.General.AST.Type as LLVM
import LLVM.General.AST.Global as Glob
import LLVM.General.AST.Constant as Const
import Control.Applicative

generateNativeSupport :: String -> [AST.Parameter] -> Maybe JassType -> Codegen ()
generateNativeSupport funcName params retType = do
  defs <- sequence [
    GlobalDefinition <$> globalNativeVariable,
    GlobalDefinition <$> globalNativeSetter]
  mapM_ addDefinition defs
  addNativeMapping funcName setterName
  where
    setterName = Name $ "__setNative__" ++ funcName
    nativeVarName = Name funcName
    nativeVarType = ptr <$> nativeType
    
    nativeType :: Codegen LLVM.Type
    nativeType = do
      llvmRetType <- toLLVMType' retType
      paramsTypes <- mapM (toLLVMType . getParamType) params
      return FunctionType {
        resultType = llvmRetType,
        argumentTypes = paramsTypes,
        isVarArg = False
      }
      
    globalNativeVariable :: Codegen LLVM.Global
    globalNativeVariable = do
      varType <- nativeVarType
      return $ globalVariableDefaults {
        name = nativeVarName,
        Glob.type' = varType,
        initializer = Just $ Null $ varType
      }
    
    globalNativeSetter :: Codegen LLVM.Global
    globalNativeSetter = do
      varType <- nativeVarType
      let ptrParName = Name "ptr"
      return $ functionDefaults {
        returnType = VoidType,
        name = setterName,
        parameters = ([LLVM.Parameter varType ptrParName []], False),
        basicBlocks = [
          BasicBlock (UnName 0) [
            Do $ Store False (ConstantOperand $ GlobalReference (ptr varType) nativeVarName)
              (LocalReference varType ptrParName) Nothing 0 []
          ] (
            Do $ Ret Nothing []
          )
        ]
      }