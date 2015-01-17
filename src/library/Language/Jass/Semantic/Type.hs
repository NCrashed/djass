module Language.Jass.Semantic.Type(
  inferType,
  typeSubsetOf,
  conform,
  isNumericType
  ) where
  
import Language.Jass.Parser.AST
import Language.Jass.Semantic.Context

-- | Infers expression type
inferType :: Expression -> JassSem s JassType
inferType expr = undefined

-- | Is b type is more general than a type
typeSubsetOf :: JassType -> JassType -> Bool
a `typeSubsetOf` b = undefined

-- | Is a and b can be implicitly casted to general type
conform :: JassType -> JassType -> Bool
a `conform` b = undefined 

-- | Is a numeric jass type
isNumericType :: JassType -> Bool
isNumericType JInteger = True
isNumericType JReal = True
isNumericType _ = False