{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Jass.Codegen.Context(
  CodegenContext(..),
  Codegen,
  runCodegen,
  newContext,
  getType,
  getCallable,
  getVariable,
  addDefinition,
  getModule
  ) where
  
import Data.HashMap.Strict as HM
import Language.Jass.Codegen.CodegenError
import Language.Jass.Parser.AST.TypeDef
import Language.Jass.Semantic.Callable
import Language.Jass.Semantic.Variable
import Control.Monad.Except
import Control.Monad.State.Strict
import qualified LLVM.General.AST as LLVM
import Control.Applicative

type Name = String

type TypeDeclarations = HashMap Name TypeDef
type CallableDeclarations = HashMap Name Callable
type VariableDeclarations = HashMap Name Variable

data CodegenContext = CodegenContext {
  contextTypes :: TypeDeclarations,
  contextCallables :: CallableDeclarations,
  contextVariables :: VariableDeclarations,
  contextModule :: LLVM.Module
}

newContext :: [TypeDef] -> [Callable] -> [Variable] -> CodegenContext
newContext types callables variables = CodegenContext {
    contextTypes = fromList $ convertToMap getTypeName types,
    contextCallables = fromList $ convertToMap getCallableName callables,
    contextVariables = fromList $ convertToMap getVarName variables,
    contextModule = LLVM.defaultModule
  } 
  where
    convertToMap getter ls = zip (fmap getter ls) ls
    
newtype Codegen a = Codegen { runCodegen_ :: ExceptT CodegenError (State CodegenContext) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenContext )
   
runCodegen :: CodegenContext -> Codegen a -> Either CodegenError a 
runCodegen context codegen = evalState (runExceptT $ runCodegen_ codegen) context

getType :: Name -> Codegen (Maybe TypeDef)
getType = getFromContext contextTypes
  
getCallable :: Name -> Codegen (Maybe Callable)
getCallable = getFromContext contextCallables

getVariable :: Name -> Codegen (Maybe Variable)
getVariable = getFromContext contextVariables

getFromContext :: (CodegenContext -> HashMap Name v) -> Name -> Codegen (Maybe v)
getFromContext getter key = do
  types <- fmap getter get 
  return $ HM.lookup key types
  
addDefinition :: LLVM.Definition -> Codegen ()
addDefinition def = do
  llvmModule <- getModule
  let llvmModule' = llvmModule { LLVM.moduleDefinitions = LLVM.moduleDefinitions llvmModule ++ [def] }
  state $ \context -> ((), context {contextModule = llvmModule'})
  
getModule :: Codegen LLVM.Module
getModule = fmap contextModule get