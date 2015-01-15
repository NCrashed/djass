{-# LANGUAGE DeriveDataTypeable #-}

module Text.Peggy.Syntax (
  Syntax,
  Definition(..),
  Expr(..),
  CharRange(..),
  CodeFragment,
  CodePart(..),
  Identifier,
  TermType,
  ) where

import Data.Data

type Syntax = [Definition]

data Definition
  = Definition Identifier TermType Expr
  deriving (Show, Eq, Typeable, Data)

data Expr
  = Terminals Bool Bool String
  | TerminalSet [CharRange]
  | TerminalCmp [CharRange]
  | TerminalAny
  | NonTerminal Identifier
  | Primitive Identifier
  | Empty
    
  | Named Identifier Expr
  
  | Sequence [Expr]
  | Choice   [Expr]
  | Many     Expr
  | Some     Expr
  | Optional Expr
  | And      Expr
  | Not      Expr
    
  | SepBy  Expr Expr
  | SepBy1 Expr Expr
  | Token  Expr
    
  | Semantic Expr CodeFragment
  deriving (Show, Eq, Typeable, Data)

data CharRange
  = CharRange Char Char
  | CharOne Char
  deriving (Show, Eq, Typeable, Data)

type CodeFragment = [CodePart]

data CodePart
  = Snippet String
  | Argument Int
  | AntiArgument Int
  | ArgPos
  | ArgSpan
  deriving (Show, Eq, Typeable, Data)

type Identifier = String
type TermType = String
