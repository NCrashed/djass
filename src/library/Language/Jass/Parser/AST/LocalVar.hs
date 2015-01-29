module Language.Jass.Parser.AST.LocalVar(
  LocalVar(..),
  getLocalVarName
  ) where
  
import Language.Jass.Parser.SourcePos
import Language.Jass.ShowIndent
import Language.Jass.JassType
import Language.Jass.Parser.AST.Expression

type IsArray = Bool
type Name = String

data LocalVar = LocalVar SrcPos IsArray JassType Name (Maybe Expression) 

instance Show LocalVar where
  show = showIndent 0
     
instance ShowIndent LocalVar where
    showIndent i (LocalVar _ False jtype name Nothing) = makeIndent i ++ "local " ++ show jtype ++ " " ++ name
    showIndent i (LocalVar _ False jtype name (Just expr)) = makeIndent i ++"local " ++ show jtype ++ " " ++ name ++ " = " ++ show expr
    showIndent i (LocalVar _ True jtype name _) = makeIndent i ++ "local " ++ show jtype ++ " array " ++ name  

instance Eq LocalVar where
  (LocalVar _ arr1 jtype1 name1 init1) == (LocalVar _ arr2 jtype2 name2 init2) =
    arr1 == arr2 && jtype1 == jtype2 && name1 == name2 && init1 == init2

-- | Returns name of local variable
getLocalVarName :: LocalVar -> String
getLocalVarName (LocalVar _ _ _ name _) = name