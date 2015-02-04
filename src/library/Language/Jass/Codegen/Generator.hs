module Language.Jass.Codegen.Generator(
  generateLLVM,
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
import LLVM.General.AST.Global
import LLVM.General.AST.Type
import Control.Monad
import Control.Monad.Error
import Control.Applicative

generateLLVM :: [TypeDef] -> [Callable] -> [Variable] -> Either SemanticError (NativesMapping, Module)
generateLLVM types callables variables = runCodegen context $ do
  --addRuntimeDefenitions
  --mapM_ (addDefinition <=< genLLVM) $ reverse types
  mapM_ genLLVM $ reverse callables
  --mapM_ (addDefinition <=< genLLVM) $ reverse variables
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
  genLLVM _ = undefined
  
instance LLVMDefinition Callable where
  genLLVM (CallableNative (NativeDecl _ _ (FunctionDecl _ fname pars retType))) =
    generateNativeSupport fname pars retType
  genLLVM (CallableFunc (AST.Function _ _ (FunctionDecl _ fname pars retType) locals stmts)) = do
    -- Init context for new function
    purgeLocalVars
    purgeNames
    _ <- purgeBlocks
    setCurrentFunction fname 
    pushNewBlock =<< generateName
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
        let valName = Name ("local_" ++ varName)
        allocName <- generateName
        let ptrRef = LocalReference (ptr llvmType) allocName
        let newBlock = BasicBlock (localBlockName varName) 
                      (preInstr ++ 
                      [allocName := Alloca llvmType Nothing 0 [],
                       Do $ Store False ptrRef val Nothing 0 [],
                       valName := Load False ptrRef Nothing 0 []])
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