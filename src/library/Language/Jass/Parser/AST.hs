{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.Jass.Parser.AST(
    ShowIndent(..),
    JassType(..),
    Name, IsConstant, IsArray, IsDebug,
    JassModule(..),
    TypeDef(..),
    GlobalVar(..),
    LocalVar(..),
    NativeDecl(..),
    FunctionDecl(..), 
    Function(..),
    BinaryOperator(..),
    UnaryOperator(..),
    Expression(..),
    Statement(..),
    setDebugStatement
    ) where

import Data.Typeable
import Control.Arrow (second)

-- | Printing with indentation
class ShowIndent a where
  -- | As show but makes indentation with specified level
  showIndent :: Int -> a -> String
  
-- | Default implementation of indentation
makeIndent :: Int -> String
makeIndent i = replicate i '\t'
  
-- | Supported types
data JassType = JInteger | JReal | JBoolean | JString | JHandle | JCode | JArray JassType | JUserDefined String
    deriving (Eq, Ord, Typeable)

instance Show JassType where
  show = showIndent 0
  
instance ShowIndent JassType where
  showIndent i JInteger = makeIndent i ++ "integer"
  showIndent i JReal = makeIndent i ++ "real"
  showIndent i JBoolean = makeIndent i ++ "boolean"
  showIndent i JString = makeIndent i ++ "string"
  showIndent i JHandle = makeIndent i ++ "handle"
  showIndent i JCode = makeIndent i ++ "code"
  showIndent i (JArray jtype) = makeIndent i ++ "array " ++ show jtype
  showIndent i (JUserDefined name) = makeIndent i ++ name
    
-- Some type synonyms
type Name = String
type IsConstant = Bool
type IsArray = Bool
type IsDebug = Bool

-- Global declarations
data JassModule = JassModule [TypeDef] [GlobalVar] [NativeDecl] [Function]
    deriving (Eq)
    
instance Show JassModule where
  show = showIndent 0
  
instance ShowIndent JassModule where
  showIndent i (JassModule typedefs globals natives functions) = 
      newlineSep (map (showIndent i) typedefs) ++ "\n" ++
      globalsString ++
      newlineSep (map (showIndent i) natives) ++ "\n" ++
      sepWith "\n\n" (map (showIndent i) functions)
      where globalsString = if null globals then "" else  "globals\n" ++ newlineSep (map (showIndent (i+1)) globals) ++ "\n" ++ makeIndent i ++ "endglobals\n"

data TypeDef = TypeDef Name JassType
    deriving (Eq)

instance Show TypeDef where
  show = showIndent 0
  
instance ShowIndent TypeDef where
    showIndent i (TypeDef name extend) = makeIndent i ++ "type " ++ name ++ " extends " ++ show extend
    
data GlobalVar = GlobalVar IsConstant IsArray JassType Name (Maybe Expression)
    deriving (Eq)

instance Show GlobalVar where
  show = showIndent 0 
  
instance ShowIndent GlobalVar where
  showIndent i (GlobalVar isConstant False jtype name Nothing) = makeIndent i ++ (if isConstant then "constant " else "") ++ show jtype ++ " " ++ name
  showIndent i (GlobalVar isConstant False jtype name (Just expr)) = makeIndent i ++ (if isConstant then "constant " else "") ++ show jtype ++ " " ++ name ++ " = " ++ show expr
  showIndent i (GlobalVar isConstant True jtype name _) = makeIndent i ++ (if isConstant then "constant " else "") ++ show jtype ++ " array " ++ name
     
data NativeDecl = NativeDecl IsConstant FunctionDecl
    deriving (Eq)

instance Show NativeDecl where
  show = showIndent 0
   
instance ShowIndent NativeDecl where
  showIndent i (NativeDecl isConstant decl) = makeIndent i ++ (if isConstant then "constant " else "") ++ "native " ++ show decl
    
data FunctionDecl = FunctionDecl Name [(JassType, Name)] (Maybe JassType)
    deriving (Eq)

instance Show FunctionDecl where
  show = showIndent 0
     
instance ShowIndent FunctionDecl where
  showIndent i (FunctionDecl name pars rtype) = makeIndent i ++ name ++ " takes " ++ params ++ " returns " ++ maybe "nothing" show rtype
      where params = if null pars then "nothing" else commaSep (map (\(t,a) -> show t ++ " " ++ a) pars)
        
data Function = Function IsConstant FunctionDecl [LocalVar] [Statement]
    deriving (Eq)

instance Show Function where
  show = showIndent 0
      
instance ShowIndent Function where
  showIndent i (Function isConstant decl locals statements) = makeIndent i ++ (if isConstant then "constant " else "") ++ "function " ++ show decl ++ "\n" ++ localsString ++ statementsString ++ makeIndent i ++ "endfunction"
    where localsString = if null locals then "" else newlineSep (map (showIndent $ i+1) locals) ++ "\n"
          statementsString = if null statements then "" else newlineSep (map (showIndent $ i+1) statements) ++ "\n"
          
data LocalVar = LocalVar IsArray JassType Name (Maybe Expression) 
    deriving (Eq)

instance Show LocalVar where
  show = showIndent 0
     
instance ShowIndent LocalVar where
    showIndent i (LocalVar False jtype name Nothing) = makeIndent i ++ "local " ++ show jtype ++ " " ++ name
    showIndent i (LocalVar False jtype name (Just expr)) = makeIndent i ++"local " ++ show jtype ++ " " ++ name ++ " = " ++ show expr
    showIndent i (LocalVar True jtype name _) = makeIndent i ++ "local " ++ show jtype ++ " array " ++ name  
    
-- Expressions
data BinaryOperator = And | Or | Equal | NotEqual | GreaterEqual | LessEqual | Greater | Less | Summ | Substract | Multiply | Divide | Reminder
    deriving (Enum, Ord, Eq, Typeable)
    
instance Show BinaryOperator where
    show And = "and"
    show Or = "or"
    show Equal = "=="
    show NotEqual = "!="
    show GreaterEqual = ">="
    show LessEqual = "<="
    show Greater = ">"
    show Less = "<"
    show Summ = "+"
    show Substract = "-"
    show Multiply = "*"
    show Divide = "/"
    show Reminder = "mod"
    
data UnaryOperator = Plus | Negation | Not
    deriving (Enum, Ord, Eq, Typeable)

instance Show UnaryOperator where
    show Plus = "+"
    show Negation = "-"
    show Not = "not"
    
data Expression where
    BinaryExpression :: BinaryOperator -> Expression -> Expression -> Expression
    UnaryExpression :: UnaryOperator -> Expression -> Expression
    ArrayReference :: Name -> Expression -> Expression
    FunctionCall :: Name -> [Expression] -> Expression
    FunctionReference :: Name -> Expression
    VariableReference :: Name -> Expression
    IntegerLiteral :: Int -> Expression
    StringLiteral :: String -> Expression
    RealLiteral :: Float -> Expression
    BoolLiteral :: Bool -> Expression
    NullLiteral :: Expression
 
instance Show Expression where
  show = showIndent 0 
     
instance ShowIndent Expression where
    showIndent i (BinaryExpression op left right) = makeIndent i ++ "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
    showIndent i (UnaryExpression op value) = makeIndent i ++ "(" ++ show op ++ " " ++ show value  ++ ")"
    showIndent i (ArrayReference name index) = makeIndent i ++ name ++ "[" ++ show index ++ "]"
    showIndent i (FunctionCall name args) = makeIndent i ++ name ++ "(" ++ commaSep (map show args) ++ ")"
    showIndent i (FunctionReference name) = makeIndent i ++ "function " ++ name
    showIndent i (VariableReference name) = makeIndent i ++ name
    showIndent i (IntegerLiteral int) = makeIndent i ++ show int
    showIndent i (StringLiteral str) = makeIndent i ++ "\"" ++ str ++ "\""
    showIndent i (RealLiteral float) = makeIndent i ++ show float
    showIndent i (BoolLiteral True) = makeIndent i ++ "true"
    showIndent i (BoolLiteral False) = makeIndent i ++ "false"
    showIndent i NullLiteral = makeIndent i ++ "null"

instance Eq Expression where
    (BinaryExpression op1 left1 right1) == (BinaryExpression op2 left2 right2) = op1 == op2 && left1 == left2 && right1 == right2
    (UnaryExpression op1 value1) == (UnaryExpression op2 value2) = op1 == op2 && value1 == value2
    (ArrayReference name1 index1) == (ArrayReference name2 index2) = name1 == name2 && index1 == index2
    (FunctionCall name1 args1) == (FunctionCall name2 args2) = name1 == name2 && args1 == args2
    (FunctionReference name1) == (FunctionReference name2) = name1 == name2
    (VariableReference name1) == (VariableReference name2) = name1 == name2
    (IntegerLiteral int1) == (IntegerLiteral int2) = int1 == int2
    (StringLiteral str1) == (StringLiteral str2) = str1 == str2
    (RealLiteral float1) == (RealLiteral float2) = float1 == float2
    (BoolLiteral bool1) == (BoolLiteral bool2) = bool1 == bool2
    NullLiteral == NullLiteral = True
    _ == _ = False
    
-- | Statements
data Statement where
    SetStatement :: IsDebug -> Name -> Expression -> Statement
    SetArrayStatement :: IsDebug -> Name -> Expression -> Expression -> Statement
    IfThenElseStatement :: IsDebug -> Expression -> [Statement] -> [(Maybe Expression, [Statement])] -> Statement
    CallStatement :: IsDebug -> Name -> [Expression] -> Statement
    LoopStatement :: IsDebug -> [Statement] -> Statement
    ExitWhenStatement :: Expression -> Statement
    ReturnStatement :: Maybe Expression -> Statement

instance Show Statement where
  show = showIndent 0
      
instance ShowIndent Statement where
    showIndent i (SetStatement dbg name expr) = makeIndent i ++ (if dbg then "debug " else "") ++ "set " ++ name ++ " = " ++ show expr
    showIndent i (SetArrayStatement dbg name index expr) = makeIndent i ++ (if dbg then "debug" else "") ++ "set " ++ name ++ "[" ++ show index ++ "] = " ++ show expr
    showIndent i (IfThenElseStatement dbg cond thenStmts elseClauses) = makeIndent i ++ (if dbg then "debug" else "") ++ "if " ++ show cond 
        ++ " then\n" ++ thenClauseString ++ elseClauseString ++ makeIndent i ++ "endif"
        where thenClauseString = if null thenStmts then "" else newlineSep (map (showIndent $ i+1) thenStmts) ++ "\n" 
              elseClauseString = if null elseClauses then "" else newlineSep (map showElseClause elseClauses) ++ "\n"
              showElseClause (Nothing, stmts) = makeIndent i ++ "else\n" ++ newlineSep (map (showIndent $ i+1) stmts)
              showElseClause (Just econd, stmts) = makeIndent i ++ "elseif " ++ show econd ++ " then\n" ++ newlineSep (map (showIndent $ i+1) stmts)  
    showIndent i (CallStatement dbg name args) = makeIndent i ++ (if dbg then "debug " else "") ++ "call " ++ name ++ "(" ++ commaSep (map show args) ++ ")"
    showIndent i (LoopStatement dbg stmts) = makeIndent i ++ (if dbg then "debug " else "") ++ "loop\n" ++ stmtsString ++ makeIndent i ++ "endloop"
      where stmtsString = if null stmts then "" else newlineSep (map (showIndent $ i+1) stmts) ++ "\n"
    showIndent i (ExitWhenStatement cond) = makeIndent i ++ "exitwhen " ++ show cond
    showIndent i (ReturnStatement Nothing) = makeIndent i ++ "return"
    showIndent i (ReturnStatement (Just expr)) = makeIndent i ++ "return " ++ show expr

instance Eq Statement where
    (SetStatement dbg1 name1 expr1) == (SetStatement dbg2 name2 expr2) = dbg1 == dbg2 && name1 == name2 && expr1 == expr2
    (SetArrayStatement dbg1 name1 index1 expr1) == (SetArrayStatement dbg2 name2 index2 expr2) = dbg1 == dbg2 && name1 == name2 && index1 == index2 && expr1 == expr2
    (IfThenElseStatement dbg1 cond1 thenStmts1 elseClauses1) == (IfThenElseStatement dbg2 cond2 thenStmts2 elseClauses2) = dbg1 == dbg2 && cond1 == cond2 && thenStmts1 == thenStmts2 && elseClauses1 == elseClauses2
    (CallStatement dbg1 name1 args1) == (CallStatement dbg2 name2 args2) = dbg1 == dbg2 && name1 == name2 && args1 == args2
    (LoopStatement dbg1 stmts1) == (LoopStatement dbg2 stmts2) = dbg1 == dbg2 && stmts1 == stmts2
    (ExitWhenStatement cond1) == (ExitWhenStatement cond2) = cond1 == cond2
    (ReturnStatement expr1) == (ReturnStatement expr2) = expr1 == expr2
    _ == _ = False
    
-- | Setting debug flag for a statement
setDebugStatement :: Bool -> Statement -> Statement
setDebugStatement flag (SetStatement _ name expr) = SetStatement flag name expr
setDebugStatement flag (SetArrayStatement _ name index expr) = SetArrayStatement flag name index expr
setDebugStatement flag (IfThenElseStatement _ cond thenStmts elseClauses) = IfThenElseStatement flag cond 
  (fmap (setDebugStatement flag) thenStmts) 
  (fmap (second $ fmap (setDebugStatement flag)) elseClauses)
setDebugStatement flag (CallStatement _ name args) = CallStatement flag name args
setDebugStatement flag (LoopStatement _ stmts) = LoopStatement flag $ fmap (setDebugStatement flag) stmts
setDebugStatement _ (ExitWhenStatement cond) = ExitWhenStatement cond
setDebugStatement _ (ReturnStatement expr) = ReturnStatement expr
 
commaSep :: [String] -> String
commaSep = sepWith ", "

newlineSep :: [String] -> String
newlineSep = sepWith "\n"

sepWith :: String -> [String] -> String
sepWith sep = go ""
    where go acc [] = acc
          go acc [x] = acc ++ x
          go acc (x:xs) = go (acc ++ x ++ sep) xs