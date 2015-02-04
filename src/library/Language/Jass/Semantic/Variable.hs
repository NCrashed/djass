module Language.Jass.Semantic.Variable(
  -- | Variable utilities
  Variable(..),
  getVarName,
  getVarPos,
  getVarConstness,
  getVarInitializator,
  getVarType,
  isVarArray,
  isGlobalVariable
  ) where

import Language.Jass.Parser.AST

-- | Variable could be global, local and as a function parameter
data Variable = VarGlobal GlobalVar | VarLocal LocalVar | VarParam Parameter 
  deriving (Eq, Show)
  
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
isVarArray (VarGlobal (GlobalVar _ _ flag _ _ _)) = flag
isVarArray (VarLocal (LocalVar _ flag _ _ _)) = flag
isVarArray (VarParam _) = False

-- | Returns True if variable is global
isGlobalVariable :: Variable -> Bool
isGlobalVariable (VarGlobal _) = True
isGlobalVariable _ = False