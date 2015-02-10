module Language.Jass.Parser.AST.JassModule(
  JassModule(..)
  ) where
  
import Language.Jass.Parser.SourcePos
import Language.Jass.ShowIndent
import Language.Jass.Parser.AST.TypeDef
import Language.Jass.Parser.AST.GlobalVar
import Language.Jass.Parser.AST.NativeDecl
import Language.Jass.Parser.AST.Function
import Language.Jass.Parser.AST.Import

-- Global declarations
data JassModule = JassModule SrcPos [Import] [TypeDef] [GlobalVar] [NativeDecl] [Function]
    
instance Show JassModule where
  show = showIndent 0
  
instance ShowIndent JassModule where
  showIndent i (JassModule _ imports typedefs globals natives functions) =
      newlineSep (map (showIndent i) imports) ++ "\n" ++ 
      newlineSep (map (showIndent i) typedefs) ++ "\n" ++
      globalsString ++
      newlineSep (map (showIndent i) natives) ++ "\n" ++
      sepWith "\n\n" (map (showIndent i) functions)
      where globalsString = if null globals then "" else  "globals\n" ++ newlineSep (map (showIndent (i+1)) globals) ++ "\n" ++ makeIndent i ++ "endglobals\n"

instance Eq JassModule where
  (JassModule _ imports1 defs1 globals1 natives1 funcs1) == (JassModule _ imports2 defs2 globals2 natives2 funcs2) =
    imports1 == imports2 && defs1 == defs2 && globals1 == globals2 && natives1 == natives2 && funcs1 == funcs2