module Language.Jass.Parser.AST.Import(
  Import(..)
  ) where
  
import Language.Jass.Parser.SourcePos
import Language.Jass.ShowIndent

data Import = Import {
  importSourcePos :: SrcPos,
  importModuleName :: String 
}

instance Show Import where
  show = showIndent 0 
  
instance ShowIndent Import where
  showIndent i import' = makeIndent i ++ "import " ++ importModuleName import'
  
instance Eq Import where
  (Import _ m1) == (Import _ m2) = m1 == m2