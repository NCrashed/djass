{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Jass.Codegen.Context(
  -- | Context core
  CodegenContext(..),
  Codegen,
  runCodegen,
  newContext,
  -- | Getting info from context
  getType,
  getCallable,
  getVariable,
  addDefinition,
  getModule,
  getCurrentFunction,
  setCurrentFunction,
  -- | Local variables
  addLocalVar,
  purgeLocalVars,
  isParameter,
  -- | Name generation
  generateName,
  generateGlobalName,
  purgeNames,
  -- | Blocks stack
  pushNewBlock,
  pushBlocks,
  appendCurrentBlock,
  finishCurrentBlock,
  purgeBlocks,
  -- | Loop utilities
  saveLoopReturn,
  getLoopReturn,
  -- | Natives mapping
  NativesMapping,
  addNativeMapping,
  getNativesMapping,
  isDefinedNative,
  module SemError
  ) where
  
import Data.HashMap.Strict as HM
import Language.Jass.Parser.AST.TypeDef
import Language.Jass.Semantic.Callable
import Language.Jass.Semantic.Variable
import Language.Jass.Semantic.SemanticError as SemError
import Control.Monad.Error
import Control.Monad.State.Strict
import qualified LLVM.General.AST as LLVM
import Control.Applicative
import Data.Word
import Safe (headMay)

type Name = String

type TypeDeclarations = HashMap Name TypeDef
type CallableDeclarations = HashMap Name Callable
type VariableDeclarations = HashMap Name Variable
type NativesMapping = HashMap Name LLVM.Name 

data CodegenContext = CodegenContext {
  contextTypes :: TypeDeclarations,
  contextCallables :: CallableDeclarations,
  contextVariables :: VariableDeclarations,
  contextLocalVariables :: VariableDeclarations,
  contextModule :: LLVM.Module,
  nameCounter :: Word,
  globalNameCounter :: Word,
  contextSavedBlocks :: [LLVM.BasicBlock],
  contextLoopReturn :: Maybe LLVM.Name,
  contextCurrentFunction :: String,
  nativesMapping :: NativesMapping
}

newContext :: [TypeDef] -> [Callable] -> [Variable] -> CodegenContext
newContext types callables variables = CodegenContext {
    contextTypes = fromList $ convertToMap getTypeName types,
    contextCallables = fromList $ convertToMap getCallableName callables,
    contextVariables = fromList $ convertToMap getVarName variables,
    contextLocalVariables = HM.empty,
    contextModule = LLVM.defaultModule,
    nameCounter = 0,
    globalNameCounter = 0,
    contextSavedBlocks = [],
    contextLoopReturn = Nothing,
    contextCurrentFunction = "",
    nativesMapping = HM.empty
  } 
  where
    convertToMap getter ls = zip (fmap getter ls) ls
    
newtype Codegen a = Codegen { runCodegen_ :: ErrorT SemanticError (State CodegenContext) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenContext, MonadError SemanticError)
   
runCodegen :: CodegenContext -> Codegen a -> Either SemanticError a 
runCodegen context codegen = evalState (runErrorT $ runCodegen_ codegen) context

getType :: Name -> Codegen (Maybe TypeDef)
getType = getFromContext contextTypes
  
getCallable :: Name -> Codegen (Maybe Callable)
getCallable = getFromContext contextCallables

getVariable :: Name -> Codegen (Maybe Variable)
getVariable name = do
  local <- getFromContext contextLocalVariables name
  case local of
    Nothing -> getFromContext contextVariables name
    Just _ -> return local

getFromContext :: (CodegenContext -> HashMap Name v) -> Name -> Codegen (Maybe v)
getFromContext getter key = do
  types <- fmap getter get 
  return $ HM.lookup key types

isDefinedNative :: Name -> Codegen Bool
isDefinedNative name = do
  mc <- getCallable name
  case mc of 
    Nothing -> return False
    Just callable -> return $ isNativeFunction callable
    
addDefinition :: LLVM.Definition -> Codegen ()
addDefinition def = do
  llvmModule <- getModule
  let llvmModule' = llvmModule { LLVM.moduleDefinitions = LLVM.moduleDefinitions llvmModule ++ [def] }
  state $ \context -> ((), context {contextModule = llvmModule'})
  
getModule :: Codegen LLVM.Module
getModule = fmap contextModule get

addLocalVar :: Variable -> Codegen ()
addLocalVar var = do
  let varName = getVarName var
  mvar' <- getVariable varName
  case mvar' of
    Just _ -> throwError $ strMsg $ "Cannot add new variable to context, already exists: " ++ varName
    Nothing -> do
      context <- get
      put $ context { contextLocalVariables = HM.insert varName var (contextLocalVariables context)}

isParameter :: Name -> Codegen Bool
isParameter varName = do
  mvar <- getVariable varName
  case mvar of
    Just (VarParam _) -> return True
    _ -> return False
    
purgeLocalVars :: Codegen ()
purgeLocalVars = do
  context <- get
  put $ context { contextLocalVariables = HM.empty }

purgeNames :: Codegen ()
purgeNames = do
  context <- get
  put $ context { nameCounter = 0 }
  
generateName :: Codegen LLVM.Name
generateName = do
  context <- get
  let i = nameCounter context
  put $ context { nameCounter = i+1 }
  return $ LLVM.UnName i

generateGlobalName :: Codegen LLVM.Name
generateGlobalName = do
  context <- get
  let i = globalNameCounter context
  put $ context { globalNameCounter = i+1 }
  return $ LLVM.Name $ "global" ++ show i
  
pushNewBlock :: LLVM.Name -> Codegen ()
pushNewBlock name = do
  context <- get
  let block = LLVM.BasicBlock name [] (LLVM.Do $ LLVM.Unreachable [])
  put $ context { contextSavedBlocks = block : contextSavedBlocks context }

pushBlocks :: [LLVM.BasicBlock] -> Codegen ()
pushBlocks blocks = do
  context <- get
  put $ context { contextSavedBlocks = blocks ++ contextSavedBlocks context }
  
getCurrentBlock :: Codegen LLVM.BasicBlock
getCurrentBlock = do
  context <- get
  let block = headMay $ contextSavedBlocks context
  case block of
    Nothing -> throwError $ strMsg "ICE: No current block"
    Just block' -> return block'
  
appendCurrentBlock :: [LLVM.Named LLVM.Instruction] -> Codegen ()
appendCurrentBlock instrs = do
  block@(LLVM.BasicBlock name blockInstrs term) <- getCurrentBlock
  unless (isBlockFinished block) $ do
    let block' = LLVM.BasicBlock name (blockInstrs ++ instrs) term
    context <- get
    put $ context { contextSavedBlocks = block' : tail (contextSavedBlocks context) } 

finishCurrentBlock :: LLVM.Terminator -> Codegen ()
finishCurrentBlock term = do
  block@(LLVM.BasicBlock name blockInstrs _) <- getCurrentBlock
  unless (isBlockFinished block) $ do
    let block' = LLVM.BasicBlock name blockInstrs (LLVM.Do term)
    context <- get
    put $ context { contextSavedBlocks = block' : tail (contextSavedBlocks context) } 

isBlockFinished :: LLVM.BasicBlock -> Bool
isBlockFinished (LLVM.BasicBlock _ _ (LLVM.Do (LLVM.Unreachable []))) = False
isBlockFinished _ = True

purgeBlocks :: Codegen [LLVM.BasicBlock]
purgeBlocks = do
  context <- get
  let blocks = contextSavedBlocks context
  put $ context { contextSavedBlocks = [] }
  return blocks 
  
saveLoopReturn :: LLVM.Name -> Codegen ()
saveLoopReturn name = do
  context <- get
  put $ context { contextLoopReturn = Just name }
  
getLoopReturn :: Codegen LLVM.Name
getLoopReturn = do
  context <- get
  case contextLoopReturn context of
    Nothing -> throwError $ strMsg "ICE: nude exitwhen is occured"
    Just nm -> return nm   
    
getCurrentFunction :: Codegen String
getCurrentFunction = fmap contextCurrentFunction get

setCurrentFunction :: String -> Codegen ()
setCurrentFunction name = do
  context <- get
  put $ context { contextCurrentFunction = name, contextLoopReturn = Nothing }
  
addNativeMapping :: String -> LLVM.Name -> Codegen ()
addNativeMapping exportName setterName = do
  context <- get
  let mapping = nativesMapping context
  case exportName `HM.lookup` mapping of
    Just _ -> throwError $ strMsg $ "ICE: tried to redefine native mapping " ++ exportName
    Nothing -> put context { nativesMapping = HM.insert exportName setterName mapping }

getNativesMapping :: Codegen NativesMapping
getNativesMapping = fmap nativesMapping get