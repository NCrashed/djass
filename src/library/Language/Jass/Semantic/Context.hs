module Language.Jass.Semantic.Context(
  -- | Variable utilities
  Variable(..),
  getVarName,
  getVarPos,
  getVarConstness,
  getVarInitializator,
  getVarType,
  isVarArray,
  -- | Utilities to operate with function and natives
  Callable(..),
  getCallableName,
  getCallablePos,
  getCallableConstness,
  getCallableParameters,
  -- | Error handling
  SemanticError(..),
  updateErrorMsg,
  -- | Context operations
  JassSem,
  newContext,
  getType,
  registerType,
  getCallable,
  registerCallable,
  getVariable,
  registerVariable,
  removeVariable
  ) where
  
import Language.Jass.Parser.AST
import qualified Data.HashTable.ST.Cuckoo as HT
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Error

data Variable = VarGlobal GlobalVar | VarLocal LocalVar | VarParam Parameter 

-- | Returns variable name
getVarName :: Variable -> String
getVarName (VarGlobal (GlobalVar _ _ _ _ name _)) = name
getVarName (VarLocal (LocalVar _ _ _ name _)) = name
getVarName (VarParam (Parameter _ _ name)) = name

-- | Returns variable source poistion
getVarPos :: Variable -> SrcPos
getVarPos (VarGlobal (GlobalVar pos _ _ _ _ _)) = pos
getVarPos (VarLocal (LocalVar pos _ _ _ _)) = pos
getVarPos (VarParam (Parameter pos _ _)) = pos

-- | Returns if variable is immutable
getVarConstness :: Variable -> Bool
getVarConstness (VarGlobal (GlobalVar _ flag _ _ _ _)) = flag
getVarConstness (VarLocal _) = False
getVarConstness (VarParam _) = False

-- | Returns variable initializator
getVarInitializator :: Variable -> Maybe Expression
getVarInitializator (VarGlobal (GlobalVar _ _ _ _ _ initalizer)) = initalizer
getVarInitializator (VarLocal (LocalVar _ _ _ _ initalizer)) = initalizer
getVarInitializator (VarParam _) = Nothing

-- | Returns variable type
getVarType :: Variable -> JassType
getVarType (VarGlobal (GlobalVar _ _ _ jtype _ _)) = jtype
getVarType (VarLocal (LocalVar _ _ jtype _ _)) = jtype
getVarType (VarParam (Parameter _ jtype _)) = jtype

-- | Returns if variable is array
isVarArray :: Variable -> Bool
isVarArray (VarGlobal (GlobalVar _ flag _ _ _ _)) = flag
isVarArray (VarLocal (LocalVar _ flag _ _ _)) = flag
isVarArray (VarParam _) = False

data Callable = CallableNative NativeDecl | CallableFunc Function

-- | Returns function or native name
getCallableName :: Callable -> String
getCallableName (CallableNative (NativeDecl _ _ funcDecl)) = getFuncDeclName funcDecl
getCallableName (CallableFunc (Function _ _ funcDecl _ _)) = getFuncDeclName funcDecl

-- | Returns source position of native or function declaration
getCallablePos :: Callable -> SrcPos
getCallablePos (CallableNative (NativeDecl pos _ _)) = pos
getCallablePos (CallableFunc (Function pos _ _ _ _)) = pos

-- | Returns constness flag of native or function
getCallableConstness :: Callable -> Bool
getCallableConstness (CallableNative (NativeDecl _ constness _)) = constness
getCallableConstness (CallableFunc (Function _ constness _ _ _)) = constness

-- | Returns formal parameters of native or function
getCallableParameters :: Callable -> [Parameter]
getCallableParameters (CallableNative (NativeDecl _ _ decl)) = getFuncDeclParameters decl
getCallableParameters (CallableFunc (Function _ _ decl _ _)) = getFuncDeclParameters decl

data SemanticError = SemanticError SrcPos String 

-- | Updating error message without changing source position
updateErrorMsg :: SemanticError -> (SrcPos -> String) -> SemanticError
updateErrorMsg (SemanticError pos _) msgFunc = SemanticError pos $ msgFunc pos
 
instance Error SemanticError where
  noMsg = strMsg ""
  strMsg = SemanticError (SrcPos "" 0 0 0)
  
type TypeDeclarations s = HT.HashTable s Name TypeDef
type FunctionDeclarations s = HT.HashTable s Name Callable
type VariableDeclarations s = HT.HashTable s Name Variable
type JassContext s = (TypeDeclarations s, FunctionDeclarations s, VariableDeclarations s)

-- | Creating empty context
newContext :: ST s (JassContext s)
newContext = do
  types <- HT.new
  funcs <- HT.new
  vars <- HT.new
  return (types, funcs, vars)
  
-- | Helper monad to track Jass symbols
type JassSem s = ErrorT SemanticError (StateT (JassContext s) (ST s))

-- | Searching type declarations in context
getType :: Name -> JassSem s (Maybe TypeDef)
getType name = do
  (tdecls, _, _) <- get
  lift.lift $ HT.lookup tdecls name

-- | Registering type in context 
registerType :: TypeDef -> JassSem s ()
registerType decl@(TypeDef _ name _) = do
 (tdecls, _, _) <- get
 lift.lift $ HT.insert tdecls name decl

-- | Searching native or user function in context
getCallable :: Name -> JassSem s (Maybe Callable)
getCallable name = do
  (_, funcs, _) <- get
  lift.lift $ HT.lookup funcs name

-- | Adds native or function to context
registerCallable :: Callable -> JassSem s ()
registerCallable callable = do
  (_, funcs, _) <- get
  let name = getCallableName callable
  lift.lift $ HT.insert funcs name callable

-- | Searches variable (global or local) in context
getVariable :: Name -> JassSem s (Maybe Variable)
getVariable name = do
  (_, _, vars) <- get
  lift.lift $ HT.lookup vars name

-- | Adds variable to context
registerVariable :: Variable -> JassSem s ()
registerVariable var = do
  (_, _, vars) <- get
  lift.lift $ HT.insert vars (getVarName var) var

-- | Removes variable from context
removeVariable :: Name -> JassSem s ()
removeVariable name = do
  (_, _, vars) <- get
  lift.lift $ HT.delete vars name