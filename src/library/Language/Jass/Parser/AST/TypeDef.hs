module Language.Jass.Parser.AST.TypeDef(
  TypeDef(..),
  getTypeName,
  getTypeBase
  ) where
  
import Language.Jass.Parser.SourcePos
import Language.Jass.ShowIndent
import Language.Jass.JassType

type Name = String
data TypeDef = TypeDef SrcPos Name JassType

instance Show TypeDef where
  show = showIndent 0
  
instance ShowIndent TypeDef where
    showIndent i (TypeDef _ name extend) = makeIndent i ++ "type " ++ name ++ " extends " ++ show extend
    
instance Eq TypeDef where
  (TypeDef _ name1 jtype1) == (TypeDef _ name2 jtype2) = name1 == name2 && jtype1 == jtype2

-- | Returns type that is used as base type for specified type
getTypeBase :: TypeDef -> JassType
getTypeBase (TypeDef _ _ t) = t

-- | Returns type name
getTypeName :: TypeDef -> Name
getTypeName (TypeDef _ n _) = n
