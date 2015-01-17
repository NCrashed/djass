{-# LANGUAGE GADTs #-}
module Language.Jass.Parser.AST.Statement(
  Statement(..),
  setDebugStatement
  ) where

import Text.Peggy (SrcPos(..))
import Language.Jass.ShowIndent
import Language.Jass.Parser.AST.Expression
import Control.Arrow (second)

type Name = String
type IsDebug = Bool

-- | Jass legal statements
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