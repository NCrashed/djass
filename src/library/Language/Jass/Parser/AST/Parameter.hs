module Language.Jass.Parser.AST.Parameter(
  Parameter(..),
  getParamName,
  getParamType,
  getParamPos
  ) where
  
import Language.Jass.Parser.SourcePos
import Language.Jass.JassType

type Name = String

data Parameter = Parameter SrcPos JassType Name

-- | Returns parameter name
getParamName :: Parameter -> Name
getParamName (Parameter _ _ name) = name

-- | Returns parameter type
getParamType :: Parameter -> JassType
getParamType (Parameter _ jtype _) = jtype

-- | Returns parameter source position
getParamPos :: Parameter -> SrcPos
getParamPos (Parameter src _ _) = src

instance Show Parameter where
  show (Parameter _ jtype name) = show jtype ++ " " ++ name

instance Eq Parameter where
  (Parameter _ jtype1 name1) == (Parameter _ jtype2 name2) = jtype1 == jtype2 && name1 == name2