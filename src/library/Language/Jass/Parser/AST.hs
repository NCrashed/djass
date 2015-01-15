{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.Jass.Parser.AST(
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

-- | Supported types
data JassType = JInteger | JReal | JBoolean | JString | JHandle | JCode | JArray JassType | JUserDefined String
    deriving (Eq, Ord, Typeable)

instance Show JassType where
    show JInteger = "integer"
    show JReal = "real"
    show JBoolean = "boolean"
    show JString = "string"
    show JHandle = "handle"
    show JCode = "code"
    show (JArray jtype) = "array " ++ show jtype
    show (JUserDefined name) = name
    
-- Some type synonyms
type Name = String
type IsConstant = Bool
type IsArray = Bool
type IsDebug = Bool

-- Global declarations
data JassModule = JassModule [TypeDef] [GlobalVar] [NativeDecl] [Function]
    deriving (Eq)
    
instance Show JassModule where
    show (JassModule typedefs globals natives functions) = 
        newlineSep (map show typedefs) ++ "\n" ++
        "globals\n" ++ newlineSep (map show globals) ++ "endglobals\n" ++
        newlineSep (map show natives) ++ "\n" ++
        sepWith "\n\n" (map show functions)
        
data TypeDef = TypeDef Name JassType
    deriving (Eq)
    
instance Show TypeDef where
    show (TypeDef name extend) = "type " ++ name ++ " extends " ++ show extend
    
data GlobalVar = GlobalVar IsConstant IsArray JassType Name (Maybe Expression)
    deriving (Eq)
    
instance Show GlobalVar where
    show (GlobalVar isConstant False jtype name Nothing) = (if isConstant then "constant " else "") ++ "global " ++ show jtype ++ " " ++ name
    show (GlobalVar isConstant False jtype name (Just expr)) = (if isConstant then "constant " else "") ++ "global " ++ show jtype ++ " " ++ name ++ " = " ++ show expr
    show (GlobalVar isConstant True jtype name _) = (if isConstant then "constant " else "") ++ "global " ++ show jtype ++ " array " ++ name
     
data NativeDecl = NativeDecl IsConstant FunctionDecl
    deriving (Eq)
    
instance Show NativeDecl where
    show (NativeDecl isConstant decl) = (if isConstant then "constant " else "") ++ "native " ++ show decl
    
data FunctionDecl = FunctionDecl Name [(JassType, Name)] (Maybe JassType)
    deriving (Eq)
    
instance Show FunctionDecl where
    show (FunctionDecl name pars rtype) = name ++ " takes " ++ params ++ " returns " ++ maybe "nothing" show rtype
        where params = if null pars then "nothing" else commaSep (map (\(t,a) -> show t ++ " " ++ show a) pars)
        
data Function = Function IsConstant FunctionDecl [LocalVar] [Statement]
    deriving (Eq)
    
instance Show Function where
    show (Function isConstant decl locals statements) = (if isConstant then "constant " else "") ++ show decl ++ "\n" ++ newlineSep (map show locals) ++ "\n" ++ newlineSep (map show statements)
    
data LocalVar = LocalVar IsArray JassType Name (Maybe Expression) 
    deriving (Eq)
    
instance Show LocalVar where
    show (LocalVar False jtype name Nothing) = "local " ++ show jtype ++ name
    show (LocalVar False jtype name (Just expr)) = "local " ++ show jtype ++ name ++ " = " ++ show expr
    show (LocalVar True jtype name _) = "local " ++ show jtype ++ " array " ++ name  
    
-- Expressions
data BinaryOperator = And | Or | Equal | NotEqual | GreaterEqual | LessEqual | Greater | Less | Summ | Substract | Multiply | Divide | Reminder
    deriving (Enum, Ord, Eq, Typeable)
    
instance Show BinaryOperator where
    show And = "&&"
    show Or = "||"
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
    show Not = "!"
    
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
    show (BinaryExpression op left right) = show left ++ " " ++ show op ++ " " ++ show right
    show (UnaryExpression op value) = show op ++ " " ++ show value  
    show (ArrayReference name index) = name ++ "[" ++ show index ++ "]"
    show (FunctionCall name args) = name ++ "(" ++ commaSep (map show args) ++ ")"
    show (FunctionReference name) = "function " ++ name
    show (VariableReference name) = name
    show (IntegerLiteral int) = show int
    show (StringLiteral str) = "\"" ++ str ++ "\""
    show (RealLiteral float) = show float
    show (BoolLiteral bool) = show bool
    show NullLiteral = "null"

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
    show (SetStatement dbg name expr) = (if dbg then "debug " else "") ++ "set " ++ name ++ " = " ++ show expr
    show (SetArrayStatement dbg name index expr) = (if dbg then "debug" else "") ++ "set " ++ name ++ "[" ++ show index ++ "] = " ++ show expr
    show (IfThenElseStatement dbg cond thenStmts elseClauses) = (if dbg then "debug" else "") ++ "if " ++ show cond 
        ++ " then\n" ++ newlineSep (map show thenStmts) ++ newlineSep (map showElseClause elseClauses) ++ "endif"
        where showElseClause (Nothing, stmts) = "else\n" ++ newlineSep (map show stmts)
              showElseClause (Just econd, stmts) = "elseif " ++ show econd ++ " then\n" ++ newlineSep (map show stmts)  
    show (CallStatement dbg name args) = (if dbg then "debug " else "") ++ "call " ++ name ++ "(" ++ commaSep (map show args) ++ ")"
    show (LoopStatement dbg stmts) = (if dbg then "debug " else "") ++ "loop\n" ++ newlineSep (map show stmts) ++ "endloop"
    show (ExitWhenStatement cond) = "exitwhen " ++ show cond
    show (ReturnStatement Nothing) = "return"
    show (ReturnStatement (Just expr)) = "return " ++ show expr

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
setDebugStatement flag (IfThenElseStatement _ cond thenStmts elseClauses) = IfThenElseStatement flag cond thenStmts elseClauses
setDebugStatement flag (CallStatement _ name args) = CallStatement flag name args
setDebugStatement flag (LoopStatement _ stmts) = LoopStatement flag stmts
setDebugStatement _ (ExitWhenStatement cond) = ExitWhenStatement cond
setDebugStatement _ (ReturnStatement expr) = ReturnStatement expr
 
commaSep :: [String] -> String
commaSep = sepWith ","

newlineSep :: [String] -> String
newlineSep = sepWith "\n"

sepWith :: String -> [String] -> String
sepWith sep = go ""
    where go acc [] = acc
          go acc [x] = acc ++ x
          go acc (x:xs) = go (acc ++ x ++ sep) xs