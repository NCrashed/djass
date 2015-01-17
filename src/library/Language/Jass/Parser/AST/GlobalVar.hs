module Language.Jass.Parser.AST.GlobalVar(
  GlobalVar(..)
  ) where
  
import Text.Peggy (SrcPos(..))
import Language.Jass.ShowIndent
import Language.Jass.JassType
import Language.Jass.Parser.AST.Expression

type IsConstant = Bool
type IsArray = Bool
type Name = String

data GlobalVar = GlobalVar SrcPos IsConstant IsArray JassType Name (Maybe Expression)

instance Show GlobalVar where
  show = showIndent 0 
  
instance ShowIndent GlobalVar where
  showIndent i (GlobalVar _ isConstant False jtype name Nothing) = makeIndent i ++ (if isConstant then "constant " else "") ++ show jtype ++ " " ++ name
  showIndent i (GlobalVar _ isConstant False jtype name (Just expr)) = makeIndent i ++ (if isConstant then "constant " else "") ++ show jtype ++ " " ++ name ++ " = " ++ show expr
  showIndent i (GlobalVar _ isConstant True jtype name _) = makeIndent i ++ (if isConstant then "constant " else "") ++ show jtype ++ " array " ++ name

instance Eq GlobalVar where
  (GlobalVar _ const1 arr1 jtype1 name1 init1) == (GlobalVar _ const2 arr2 jtype2 name2 init2) =
    const1 == const2 && arr1 == arr2 && jtype1 == jtype2 && name1 == name2 && init1 == init2