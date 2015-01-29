module Language.Jass.Parser.AST.NativeDecl(
  NativeDecl(..)
  ) where

import Language.Jass.Parser.SourcePos
import Language.Jass.ShowIndent
import Language.Jass.Parser.AST.FunctionDecl

type IsConstant = Bool
data NativeDecl = NativeDecl SrcPos IsConstant FunctionDecl

instance Show NativeDecl where
  show = showIndent 0
   
instance ShowIndent NativeDecl where
  showIndent i (NativeDecl _ isConstant decl) = makeIndent i ++ (if isConstant then "constant " else "") ++ "native " ++ show decl
    
instance Eq NativeDecl where
  (NativeDecl _ const1 funcdec1) == (NativeDecl _ const2 funcdec2) = 
    const1 == const2 && funcdec1 == funcdec2