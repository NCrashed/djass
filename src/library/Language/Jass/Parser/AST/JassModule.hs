module Language.Jass.Parser.AST.JassModule(
  JassModule(..)
  ) where
  
import Text.Peggy (SrcPos(..))
import Language.Jass.ShowIndent
import Language.Jass.Parser.AST.TypeDef
import Language.Jass.Parser.AST.GlobalVar
import Language.Jass.Parser.AST.NativeDecl
import Language.Jass.Parser.AST.Function

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