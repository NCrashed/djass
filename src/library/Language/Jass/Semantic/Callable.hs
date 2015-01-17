module Language.Jass.Semantic.Callable(
  -- | Utilities to operate with function and natives
  Callable(..),
  getCallableName,
  getCallablePos,
  getCallableConstness,
  getCallableParameters,
  getCallableReturnType
  ) where

import Language.Jass.Parser.AST
 
-- | Holds function or native
data Callable = CallableNative NativeDecl | CallableFunc Function
  deriving (Eq, Show)
  
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

-- | Returns function or native return type
getCallableReturnType :: Callable -> Maybe JassType
getCallableReturnType (CallableNative (NativeDecl _ _ decl)) = getFuncDeclReturnType decl
getCallableReturnType (CallableFunc (Function _ _ decl _ _)) = getFuncDeclReturnType decl
  