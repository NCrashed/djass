module Language.Jass.Semantic.Context(
  -- | Context operations
  JassSem,
  newContext,
  getType,
  registerType,
  getCallable,
  registerCallable,
  getVariable,
  registerVariable,
  removeVariable,
  
  module Reexports
  ) where

import Language.Jass.Parser.AST
import Language.Jass.Semantic.Callable as Reexports
import Language.Jass.Semantic.Variable as Reexports
import Language.Jass.Semantic.SemanticError  as Reexports
import qualified Data.HashTable.ST.Cuckoo as HT
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Error

type Name = String

-- | Hash table with type declarations mapping 
type TypeDeclarations s = HT.HashTable s Name TypeDef
-- | Hash table with function/natives declarations mapping
type FunctionDeclarations s = HT.HashTable s Name Callable
-- | Hash table with variable declarations (including locals and function parameters)
type VariableDeclarations s = HT.HashTable s Name Variable
-- | Context is a symbol table
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