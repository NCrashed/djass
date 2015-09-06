{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Jass.Semantic.Context(
  -- | Context operations
    JassSem
  , freezeContext
  , getCallable
  , getType
  , getVariable
  , newContext
  , registerCallable
  , registerType
  , registerVariable
  , removeVariable
  , runJassSem
  ,  module ReExports
  ) where

import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State.Strict
import Language.Jass.Parser.AST
import Language.Jass.Semantic.Callable as ReExports
import Language.Jass.Semantic.SemanticError  as ReExports
import Language.Jass.Semantic.Variable as ReExports
import qualified Data.HashTable.Class as HT(toList)
import qualified Data.HashTable.ST.Cuckoo as HT

type Name = String

-- | Hash table with type declarations mapping 
type TypeDeclarations s = HT.HashTable s Name TypeDef
-- | Hash table with function/natives declarations mapping
type CallableDeclarations s = HT.HashTable s Name Callable
-- | Hash table with variable declarations (including locals and function parameters)
type VariableDeclarations s = HT.HashTable s Name Variable
-- | Context is a symbol table
type JassContext s = (TypeDeclarations s, CallableDeclarations s, VariableDeclarations s)

-- | Creating empty context
newContext :: ST s (JassContext s)
newContext = do
  types <- HT.new
  funcs <- HT.new
  vars <- HT.new
  return (types, funcs, vars)
  
-- | Helper monad to track Jass symbols
newtype JassSem s a = JassSem { unJassSem :: ExceptT SemanticError (StateT (JassContext s) (ST s)) a }
  deriving (Functor, Applicative, Monad, MonadState (JassContext s), MonadError SemanticError)

-- | Explicit use of ST action in JassSem monad
liftST :: ST s a -> JassSem s a 
liftST = JassSem . lift . lift 

-- | Perform JassSem computation
runJassSem :: JassSem s a -> ST s (Either SemanticError a)
runJassSem f = do 
  cntx <- newContext
  evalStateT (runExceptT $ unJassSem f) cntx

-- runST $ evalStateT (runErrorT $ mapM_ check mods >> freezeContext) =<< newContext
-- | Searching type declarations in context
getType :: Name -> JassSem s (Maybe TypeDef)
getType name = do
  (tdecls, _, _) <- get
  liftST $ HT.lookup tdecls name

-- | Registering type in context 
registerType :: TypeDef -> JassSem s ()
registerType decl@(TypeDef _ name _) = do
 (tdecls, _, _) <- get
 liftST $ HT.insert tdecls name decl

-- | Searching native or user function in context
getCallable :: Name -> JassSem s (Maybe Callable)
getCallable name = do
  (_, funcs, _) <- get
  liftST $ HT.lookup funcs name

-- | Adds native or function to context
registerCallable :: Callable -> JassSem s ()
registerCallable callable = do
  (_, funcs, _) <- get
  let name = getCallableName callable
  liftST $ HT.insert funcs name callable

-- | Searches variable (global or local) in context
getVariable :: Name -> JassSem s (Maybe Variable)
getVariable name = do
  (_, _, vars) <- get
  liftST $ HT.lookup vars name

-- | Adds variable to context
registerVariable :: Variable -> JassSem s ()
registerVariable var = do
  (_, _, vars) <- get
  liftST $ HT.insert vars (getVarName var) var

-- | Removes variable from context
removeVariable :: Name -> JassSem s ()
removeVariable name = do
  (_, _, vars) <- get
  liftST $ HT.delete vars name

-- | Extracts all values from context
freezeContext :: JassSem s ([TypeDef], [Callable], [Variable])
freezeContext = do
  (htypes, hfuncs, hvars) <- get
  types <- liftST $ HT.toList htypes
  funcs <- liftST $ HT.toList hfuncs
  vars <- liftST $ HT.toList hvars
  return (fmap snd types, fmap snd funcs, fmap snd vars)
