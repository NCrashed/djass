{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.Jass.Parser.AST.Expression(
  Expression(..),
  BinaryOperator(..),
  UnaryOperator(..),
  getExpressionPos,
  isArithmeticOperator,
  isRelationalOperator
  ) where

import Language.Jass.Parser.SourcePos
import Language.Jass.ShowIndent
import Data.Typeable

data BinaryOperator = And | Or | Equal | NotEqual | GreaterEqual | LessEqual | Greater | Less | Summ | Substract | Multiply | Divide | Reminder
    deriving (Enum, Ord, Eq, Typeable)

-- | True for +,-,*,/,mod
isArithmeticOperator :: BinaryOperator -> Bool
isArithmeticOperator Summ = True
isArithmeticOperator Substract = True
isArithmeticOperator Multiply = True
isArithmeticOperator Divide = True
isArithmeticOperator Reminder = True
isArithmeticOperator _ = False

-- | True for ==,!=,<,>,<=,>=
isRelationalOperator :: BinaryOperator -> Bool
isRelationalOperator Equal = True
isRelationalOperator NotEqual = True
isRelationalOperator GreaterEqual = True
isRelationalOperator LessEqual = True
isRelationalOperator Greater = True
isRelationalOperator Less = True
isRelationalOperator _ = False

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

type Name = String
 
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
  