module Language.Jass.Codegen.Generator(
  generateLLVM,
  globalsInitializerFuncName,
  NativesMapping
  ) where
  
import Language.Jass.Codegen.Context
import Language.Jass.Codegen.Type
import Language.Jass.Codegen.Statement
import Language.Jass.Codegen.Expression
import Language.Jass.Codegen.Native
import Language.Jass.Parser.AST as AST
import Language.Jass.Semantic.Callable
import Language.Jass.Semantic.Variable
import LLVM.General.AST as LLVM
import LLVM.General.AST.Global as Global
import LLVM.General.AST.Constant
import LLVM.General.AST.Type
import LLVM.General.AST.Linkage
import Control.Monad
import Control.Monad.Error

generateLLVM :: [TypeDef] -> [Callable] -> [Variable] -> Either SemanticError (NativesMapping, Module)
generateLLVM types callables variables = runCodegen context $ do
  --addRuntimeDefenitions
  --mapM_ genLLVM $ reverse types
  mapM_ genLLVM $ reverse variables
  addDefinition =<< genGlobalInitializersFunc
  mapM_ genLLVM $ reverse callables
  mapping <- getNativesMapping
  module' <- getModule
  return (mapping, module')
  where context = newContext types callables variables
  
class LLVMDefinition a where
  genLLVM :: a -> Codegen ()  

--instance LLVMDefinition TypeDef where
--  genLLVM (TypeDef _ name tp) 
--    | isBasicType tp = return $ TypeDefinition (Name name) $ Just (getBasicType tp)
--    | JArray elt <- tp = return $ TypeDefinition (Name name) $ Just $ ArrayType arraySize (getBasicType elt) 

instance LLVMDefinition Variable where
  genLLVM (VarGlobal (GlobalVar _ isConst False jt varName Nothing)) = do
    initVal <- defaultValue jt
    genGlobal jt varName isConst [] (Just initVal)
  genLLVM (VarGlobal (GlobalVar _ isConst False jt varName (Just expr))) = do
    initVal <- defaultValue jt
    (exprName, exprInstrs) <- genLLVMExpression expr
    llvmType <- toLLVMType jt   
    genGlobal jt varName isConst (exprInstrs ++ [
        Do $ Store False (ConstantOperand $ GlobalReference (ptr llvmType) (Name varName)) 
          (LocalReference llvmType exprName) Nothing 0 []
      ]) (Just initVal)
  genLLVM (VarGlobal (GlobalVar _ isConst True jt varName Nothing)) = do
    initVal <- defaultValue (JArray jt)
    genGlobal jt varName isConst [] (Just initVal)
  genLLVM (VarGlobal (GlobalVar pos _ True _ _ (Just _))) = 
    throwError $ SemanticError pos "ICE: cannot generate variable with array initializer" 
  genLLVM _ = throwError $ strMsg "ICE: cannot generate code for non-global vars at top level" 

-- | Generates global variable
genGlobal :: JassType -> String -> Bool -> [Named Instruction] -> Maybe Constant -> Codegen ()
genGlobal jt varName isConst instrs initVal = do
  llvmType <- toLLVMType jt
  addGlobalInitializer instrs
  addDefinition $ GlobalDefinition $ globalVariableDefaults {
    name = Name varName,
    isConstant = isConst,
    Global.type' = llvmType,
    initializer = initVal,
    linkage = Private -- | TODO: when would linking modules, check this
  }
  
-- | Returns name of function that sets initial values to all globals
globalsInitializerFuncName :: String
globalsInitializerFuncName = "__jass__initGlobals"

-- | Generates function that sets initial values of global variables
genGlobalInitializersFunc :: Codegen Definition
genGlobalInitializersFunc = do
  initInstrs <- getGlobalsInitializers
  return $ GlobalDefinition $ functionDefaults {
      name = Name globalsInitializerFuncName
    , parameters = ([], False)
    , returnType = VoidType
    , basicBlocks = [BasicBlock (Name "entry_block") 
        initInstrs
        (Do $ Ret Nothing [])
      ]
  }
  
instance LLVMDefinition Callable where
  genLLVM (CallableNative (NativeDecl _ _ (FunctionDecl _ fname pars retType))) =
    generateNativeSupport fname pars retType
  genLLVM (CallableFunc (AST.Function _ _ (FunctionDecl _ fname pars retType) locals stmts)) = do
    -- Init context for new function
    purgeLocalVars
    purgeNames
    setCurrentFunction fname 
    forM_ pars $ addLocalVar . VarParam
    forM_ locals $ addLocalVar . VarLocal
    
    -- Generating 
    proto <- genFunctionHeader fname pars retType
    blocks <- genBasicBlocks
    addDefinition $ GlobalDefinition $ proto { basicBlocks = blocks } 
    where
      localBlockName varName = Name $ "block_local_" ++ varName
      genBasicBlocks :: Codegen [BasicBlock]
      genBasicBlocks = do
        (entryBlockName, bodyBlocks) <- genBodyBlocks stmts
        (_, localsBlocks) <- foldM genLocal (entryBlockName, []) $ reverse locals
        return $ localsBlocks ++ bodyBlocks
        
      -- | Generates local block, attaches it to previous block and saves in accumulator
      genLocal :: (Name, [BasicBlock]) -> LocalVar -> Codegen (Name, [BasicBlock])
      genLocal (nextBlock, acc) (LocalVar _ False jt varName Nothing) = do 
        initVal <- defaultValue jt
        genLocal' jt varName [] (ConstantOperand initVal) nextBlock acc 
      genLocal (nextBlock, acc) (LocalVar _ False jt varName (Just expr)) = do
        (exprName, exprInstrs) <- genLLVMExpression expr
        llvmType <- toLLVMType jt
        genLocal' jt varName exprInstrs (LocalReference llvmType exprName) nextBlock acc 
      genLocal (nextBlock, acc) (LocalVar _ True jt varName Nothing) = do
        initVal <- defaultValue (JArray jt)
        genLocal' jt varName [] (ConstantOperand initVal) nextBlock acc 
      genLocal _ (LocalVar _ True _ _ (Just _)) = 
        throwError $ strMsg "ICE: cannot generate code for array expression at local variable initializator"

      genLocal' jt varName preInstr val nextBlock acc = do  
        llvmType <- toLLVMType jt
        let newBlock = BasicBlock (localBlockName varName) 
                      (preInstr ++ 
                      [Name varName := Alloca llvmType Nothing 0 [],
                       Do $ Store False (LocalReference (ptr llvmType) (Name varName)) val Nothing 0 []])
                      (Do $ Br nextBlock [])
        return (localBlockName varName, newBlock:acc)
        
-- | Generates prototype for function and natives
genFunctionHeader :: String -> [AST.Parameter] -> Maybe JassType -> Codegen LLVM.Global
genFunctionHeader fname pars retType = do 
  llvmPars <- mapM convParam pars
  llvmRetType <- maybe (return VoidType) toLLVMType retType
  return $ functionDefaults {
      name = Name fname
    , parameters = (llvmPars, False)
    , returnType = llvmRetType
    , basicBlocks = []
  }
  
-- | Converts function parameter from custom AST to LLVM AST    
convParam :: AST.Parameter -> Codegen LLVM.Parameter
convParam (AST.Parameter _ pt pname) = do
  llvmt <- toLLVMType pt
  return $ LLVM.Parameter llvmt (Name pname) []
        
--addRuntimeDefenitions :: Codegen ()
--addRuntimeDefenitions = 
--  addDefinition $ LLVM.GlobalDefinition $ functionDefaults {
--      name = Name "malloc"
--    , parameters = ([LLVM.Parameter i32 (Name "size") []], False)
--    , returnType = ptr i8
--    , basicBlocks = []
--  }