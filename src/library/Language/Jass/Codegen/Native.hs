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
import LLVM.General.AST.Linkage
import LLVM.General.AST.Visibility
import LLVM.General.AST.CallingConvention

generateNativeSupport :: String -> [AST.Parameter] -> Maybe JassType -> Codegen ()
generateNativeSupport funcName params retType = do
  defs <- sequence [
    GlobalDefinition <$> globalNativeVariable,
    GlobalDefinition <$> globalNativeSetter,
    GlobalDefinition <$> globalNativeCaller]
  mapM_ addDefinition defs
  addNativeMapping funcName setterName
  where
    setterName = Name $ "$__jass__native__set__" ++ funcName
    nativeVarName = Name $ "$__jass__native__" ++ funcName ++ "Var"
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
        visibility = Hidden,
        linkage = Private,
        name = nativeVarName,
        Glob.type' = varType,
        initializer = Just $ Null varType
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
    
    toLLVMParam (AST.Parameter _ t n) = do
      t' <- toLLVMType t
      return $ LLVM.Parameter t' (Name n) []
      
    globalNativeCaller = do
      llvmResType <- toLLVMType' retType
      pars <- mapM toLLVMParam params
      let args = fmap (\(LLVM.Parameter t n _) -> LocalReference t n) pars `zip` repeat [] 
      funcType <- nativeType
      return $ functionDefaults {
        returnType = llvmResType,
        name = Name funcName,
        linkage = Private,
        visibility = Hidden,
        parameters = (pars, False),
        basicBlocks = if llvmResType == VoidType then [
          BasicBlock (Name "entry") [
            Name "fptr" := Load False (ConstantOperand $ GlobalReference (ptr $ ptr funcType) nativeVarName) Nothing 0 [],
            Do $ Call False C [] (Right $ LocalReference (ptr funcType) $ Name "fptr") args [] []
          ] (Do $ Ret Nothing [])
        ] else [
          BasicBlock (Name "entry") [
            Name "fptr" := Load False (ConstantOperand $ GlobalReference (ptr $ ptr funcType) nativeVarName) Nothing 0 [],
            Name "res" := Call False C [] (Right $ LocalReference (ptr funcType) $ Name "fptr") args [] []
          ] (Do $ Ret (Just $ LocalReference llvmResType $ Name "res") [])
        ]
      }