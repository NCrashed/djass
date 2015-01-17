module Language.Jass.Parser.AST.FunctionDecl(
  FunctionDecl(..),
  getFuncDeclName,
  getFuncDeclParameters,
  getFuncDeclReturnType
  ) where
  
import Text.Peggy (SrcPos(..))
import Language.Jass.ShowIndent
import Language.Jass.JassType
import Language.Jass.Parser.AST.Parameter

type Name = String
data FunctionDecl = FunctionDecl SrcPos Name [Parameter] (Maybe JassType)

instance Show FunctionDecl where
  show = showIndent 0
     
instance ShowIndent FunctionDecl where
  showIndent i (FunctionDecl _ name pars rtype) = makeIndent i ++ name ++ " takes " ++ params ++ " returns " ++ maybe "nothing" show rtype
      where params = if null pars then "nothing" else commaSep (map show pars)

instance Eq FunctionDecl where
  (FunctionDecl _ name1 pars1 ret1) == (FunctionDecl _ name2 pars2 ret2) =
    name1 == name2 && pars1 == pars2 && ret1 == ret2

-- | Returns function declaration name, helper
getFuncDeclName :: FunctionDecl -> String
getFuncDeclName (FunctionDecl _ name _ _) = name

-- | Returns parameters of function declaration
getFuncDeclParameters :: FunctionDecl -> [Parameter]
getFuncDeclParameters (FunctionDecl _ _ pars _) = pars

-- | Return function declaration return type   
getFuncDeclReturnType :: FunctionDecl -> Maybe JassType
getFuncDeclReturnType (FunctionDecl _ _ _ retType) = retType