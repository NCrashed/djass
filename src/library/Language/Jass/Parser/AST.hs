{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.Jass.Parser.AST(
    SrcPos(..),
    ShowIndent(..),
    JassType(..),
    Name, IsConstant, IsArray, IsDebug,
    JassModule(..),
    TypeDef(..),
    GlobalVar(..),
    LocalVar(..),
    NativeDecl(..),
    Parameter(..),
    getParamName,
    getParamType,
    getParamPos,
    FunctionDecl(..), 
    getFuncDeclName,
    getFuncDeclParameters,
    Function(..),
    BinaryOperator(..),
    UnaryOperator(..),
    Expression(..),
    getExpressionPos,
    Statement(..),
    setDebugStatement
    ) where

import Data.Typeable
import Control.Arrow (second)
import Text.Peggy (SrcPos(..))

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
data JassModule = JassModule SrcPos [TypeDef] [GlobalVar] [NativeDecl] [Function]
    
instance Show JassModule where
  show = showIndent 0
  
instance ShowIndent JassModule where
  showIndent i (JassModule _ typedefs globals natives functions) = 
      newlineSep (map (showIndent i) typedefs) ++ "\n" ++
      globalsString ++
      newlineSep (map (showIndent i) natives) ++ "\n" ++
      sepWith "\n\n" (map (showIndent i) functions)
      where globalsString = if null globals then "" else  "globals\n" ++ newlineSep (map (showIndent (i+1)) globals) ++ "\n" ++ makeIndent i ++ "endglobals\n"

instance Eq JassModule where
  (JassModule _ defs1 globals1 natives1 funcs1) == (JassModule _ defs2 globals2 natives2 funcs2) =
    defs1 == defs2 && globals1 == globals2 && natives1 == natives2 && funcs1 == funcs2
    
data TypeDef = TypeDef SrcPos Name JassType

instance Show TypeDef where
  show = showIndent 0
  
instance ShowIndent TypeDef where
    showIndent i (TypeDef _ name extend) = makeIndent i ++ "type " ++ name ++ " extends " ++ show extend
    
instance Eq TypeDef where
  (TypeDef _ name1 jtype1) == (TypeDef _ name2 jtype2) = name1 == name2 && jtype1 == jtype2
  
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
    
data NativeDecl = NativeDecl SrcPos IsConstant FunctionDecl

instance Show NativeDecl where
  show = showIndent 0
   
instance ShowIndent NativeDecl where
  showIndent i (NativeDecl _ isConstant decl) = makeIndent i ++ (if isConstant then "constant " else "") ++ "native " ++ show decl
    
instance Eq NativeDecl where
  (NativeDecl _ const1 funcdec1) == (NativeDecl _ const2 funcdec2) = 
    const1 == const2 && funcdec1 == funcdec2

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
    BinaryExpression :: SrcPos -> BinaryOperator -> Expression -> Expression -> Expression
    UnaryExpression :: SrcPos -> UnaryOperator -> Expression -> Expression
    ArrayReference :: SrcPos -> Name -> Expression -> Expression
    FunctionCall :: SrcPos -> Name -> [Expression] -> Expression
    FunctionReference :: SrcPos -> Name -> Expression
    VariableReference :: SrcPos -> Name -> Expression
    IntegerLiteral :: SrcPos -> Int -> Expression
    StringLiteral :: SrcPos -> String -> Expression
    RealLiteral :: SrcPos -> Float -> Expression
    BoolLiteral :: SrcPos -> Bool -> Expression
    NullLiteral :: SrcPos -> Expression
 
instance Show Expression where
  show = showIndent 0 
     
instance ShowIndent Expression where
    showIndent i (BinaryExpression _ op left right) = makeIndent i ++ "(" ++ show left ++ " " ++ show op ++ " " ++ show right ++ ")"
    showIndent i (UnaryExpression _ op value) = makeIndent i ++ "(" ++ show op ++ " " ++ show value  ++ ")"
    showIndent i (ArrayReference _ name index) = makeIndent i ++ name ++ "[" ++ show index ++ "]"
    showIndent i (FunctionCall _ name args) = makeIndent i ++ name ++ "(" ++ commaSep (map show args) ++ ")"
    showIndent i (FunctionReference _ name) = makeIndent i ++ "function " ++ name
    showIndent i (VariableReference _ name) = makeIndent i ++ name
    showIndent i (IntegerLiteral _ int) = makeIndent i ++ show int
    showIndent i (StringLiteral _ str) = makeIndent i ++ "\"" ++ str ++ "\""
    showIndent i (RealLiteral _ float) = makeIndent i ++ show float
    showIndent i (BoolLiteral _ True) = makeIndent i ++ "true"
    showIndent i (BoolLiteral _ False) = makeIndent i ++ "false"
    showIndent i (NullLiteral _) = makeIndent i ++ "null"

instance Eq Expression where
    (BinaryExpression _ op1 left1 right1) == (BinaryExpression _ op2 left2 right2) = op1 == op2 && left1 == left2 && right1 == right2
    (UnaryExpression _ op1 value1) == (UnaryExpression _ op2 value2) = op1 == op2 && value1 == value2
    (ArrayReference _ name1 index1) == (ArrayReference _ name2 index2) = name1 == name2 && index1 == index2
    (FunctionCall _ name1 args1) == (FunctionCall _ name2 args2) = name1 == name2 && args1 == args2
    (FunctionReference _ name1) == (FunctionReference _ name2) = name1 == name2
    (VariableReference _ name1) == (VariableReference _ name2) = name1 == name2
    (IntegerLiteral _ int1) == (IntegerLiteral _ int2) = int1 == int2
    (StringLiteral _ str1) == (StringLiteral _ str2) = str1 == str2
    (RealLiteral _ float1) == (RealLiteral _ float2) = float1 == float2
    (BoolLiteral _ bool1) == (BoolLiteral _ bool2) = bool1 == bool2
    (NullLiteral _) == (NullLiteral _) = True
    _ == _ = False

-- | Returns source position of expression
getExpressionPos :: Expression -> SrcPos
getExpressionPos (BinaryExpression src _ _ _) = src
getExpressionPos (UnaryExpression src _ _) = src
getExpressionPos (ArrayReference src _ _) = src
getExpressionPos (FunctionCall src _ _) = src
getExpressionPos (FunctionReference src _) = src
getExpressionPos (VariableReference src _) = src
getExpressionPos (IntegerLiteral src _) = src
getExpressionPos (StringLiteral src _) = src
getExpressionPos (RealLiteral src _) = src
getExpressionPos (BoolLiteral src _) = src
getExpressionPos (NullLiteral src) = src    

-- | Statements
data Statement where
    SetStatement :: SrcPos -> IsDebug -> Name -> Expression -> Statement
    SetArrayStatement :: SrcPos -> IsDebug -> Name -> Expression -> Expression -> Statement
    IfThenElseStatement :: SrcPos -> IsDebug -> Expression -> [Statement] -> [(Maybe Expression, [Statement])] -> Statement
    CallStatement :: SrcPos -> IsDebug -> Name -> [Expression] -> Statement
    LoopStatement :: SrcPos -> IsDebug -> [Statement] -> Statement
    ExitWhenStatement :: SrcPos -> Expression -> Statement
    ReturnStatement :: SrcPos -> Maybe Expression -> Statement

instance Show Statement where
  show = showIndent 0
      
instance ShowIndent Statement where
    showIndent i (SetStatement _ dbg name expr) = makeIndent i ++ (if dbg then "debug " else "") ++ "set " ++ name ++ " = " ++ show expr
    showIndent i (SetArrayStatement _ dbg name index expr) = makeIndent i ++ (if dbg then "debug" else "") ++ "set " ++ name ++ "[" ++ show index ++ "] = " ++ show expr
    showIndent i (IfThenElseStatement _ dbg cond thenStmts elseClauses) = makeIndent i ++ (if dbg then "debug" else "") ++ "if " ++ show cond 
        ++ " then\n" ++ thenClauseString ++ elseClauseString ++ makeIndent i ++ "endif"
        where thenClauseString = if null thenStmts then "" else newlineSep (map (showIndent $ i+1) thenStmts) ++ "\n" 
              elseClauseString = if null elseClauses then "" else newlineSep (map showElseClause elseClauses) ++ "\n"
              showElseClause (Nothing, stmts) = makeIndent i ++ "else\n" ++ newlineSep (map (showIndent $ i+1) stmts)
              showElseClause (Just econd, stmts) = makeIndent i ++ "elseif " ++ show econd ++ " then\n" ++ newlineSep (map (showIndent $ i+1) stmts)  
    showIndent i (CallStatement _ dbg name args) = makeIndent i ++ (if dbg then "debug " else "") ++ "call " ++ name ++ "(" ++ commaSep (map show args) ++ ")"
    showIndent i (LoopStatement _ dbg stmts) = makeIndent i ++ (if dbg then "debug " else "") ++ "loop\n" ++ stmtsString ++ makeIndent i ++ "endloop"
      where stmtsString = if null stmts then "" else newlineSep (map (showIndent $ i+1) stmts) ++ "\n"
    showIndent i (ExitWhenStatement _ cond) = makeIndent i ++ "exitwhen " ++ show cond
    showIndent i (ReturnStatement _ Nothing) = makeIndent i ++ "return"
    showIndent i (ReturnStatement _ (Just expr)) = makeIndent i ++ "return " ++ show expr

instance Eq Statement where
    (SetStatement _ dbg1 name1 expr1) == (SetStatement _ dbg2 name2 expr2) = dbg1 == dbg2 && name1 == name2 && expr1 == expr2
    (SetArrayStatement _ dbg1 name1 index1 expr1) == (SetArrayStatement _ dbg2 name2 index2 expr2) = dbg1 == dbg2 && name1 == name2 && index1 == index2 && expr1 == expr2
    (IfThenElseStatement _ dbg1 cond1 thenStmts1 elseClauses1) == (IfThenElseStatement _ dbg2 cond2 thenStmts2 elseClauses2) = dbg1 == dbg2 && cond1 == cond2 && thenStmts1 == thenStmts2 && elseClauses1 == elseClauses2
    (CallStatement _ dbg1 name1 args1) == (CallStatement _ dbg2 name2 args2) = dbg1 == dbg2 && name1 == name2 && args1 == args2
    (LoopStatement _ dbg1 stmts1) == (LoopStatement _ dbg2 stmts2) = dbg1 == dbg2 && stmts1 == stmts2
    (ExitWhenStatement _ cond1) == (ExitWhenStatement _ cond2) = cond1 == cond2
    (ReturnStatement _ expr1) == (ReturnStatement _ expr2) = expr1 == expr2
    _ == _ = False
    
-- | Setting debug flag for a statement
setDebugStatement :: Bool -> Statement -> Statement
setDebugStatement flag (SetStatement src _ name expr) = SetStatement src flag name expr
setDebugStatement flag (SetArrayStatement src _ name index expr) = SetArrayStatement src flag name index expr
setDebugStatement flag (IfThenElseStatement src _ cond thenStmts elseClauses) = IfThenElseStatement src flag cond 
  (fmap (setDebugStatement flag) thenStmts) 
  (fmap (second $ fmap (setDebugStatement flag)) elseClauses)
setDebugStatement flag (CallStatement src _ name args) = CallStatement src flag name args
setDebugStatement flag (LoopStatement src _ stmts) = LoopStatement src flag $ fmap (setDebugStatement flag) stmts
setDebugStatement _ (ExitWhenStatement src cond) = ExitWhenStatement src cond
setDebugStatement _ (ReturnStatement src expr) = ReturnStatement src expr
 
commaSep :: [String] -> String
commaSep = sepWith ", "

newlineSep :: [String] -> String
newlineSep = sepWith "\n"

sepWith :: String -> [String] -> String
sepWith sep = go ""
    where go acc [] = acc
          go acc [x] = acc ++ x
          go acc (x:xs) = go (acc ++ x ++ sep) xs