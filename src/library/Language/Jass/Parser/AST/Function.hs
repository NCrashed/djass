module Language.Jass.Parser.AST.Function(
  Function(..)
  ) where
  
import Language.Jass.Parser.SourcePos
import Language.Jass.ShowIndent
import Language.Jass.Parser.AST.FunctionDecl
import Language.Jass.Parser.AST.LocalVar
import Language.Jass.Parser.AST.Statement

type IsConstant = Bool

data Function = Function SrcPos IsConstant FunctionDecl [LocalVar] [Statement]

instance Show Function where
  show = showIndent 0
      
instance ShowIndent Function where
  showIndent i (Function _ isConstant decl locals statements) = makeIndent i ++ (if isConstant then "constant " else "") ++ "function " ++ show decl ++ "\n" ++ localsString ++ statementsString ++ makeIndent i ++ "endfunction"
    where localsString = if null locals then "" else newlineSep (map (showIndent $ i+1) locals) ++ "\n"
          statementsString = if null statements then "" else newlineSep (map (showIndent $ i+1) statements) ++ "\n"

instance Eq Function where
  (Function _ const1 funcdec1 locals1 stmts1) == (Function _ const2 funcdec2 locals2 stmts2) = 
    const1 == const2 && funcdec1 == funcdec2 && locals1 == locals2 && stmts1 == stmts2