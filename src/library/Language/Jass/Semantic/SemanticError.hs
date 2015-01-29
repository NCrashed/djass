module Language.Jass.Semantic.SemanticError(
  -- | Error handling
  SemanticError(..),
  updateErrorMsg,
  strMsg,
  noMsg
  ) where
  
import Language.Jass.Parser.AST

-- | Semantic error holds source position and a message
data SemanticError = SemanticError SrcPos String 

-- | Updating error message without changing source position
updateErrorMsg :: SemanticError -> (SrcPos -> String) -> SemanticError
updateErrorMsg (SemanticError pos _) msgFunc = SemanticError pos $ msgFunc pos

-- | Creating error without source position
strMsg :: String -> SemanticError
strMsg = SemanticError noPos

-- | Empty error
noMsg :: SemanticError
noMsg = strMsg ""

instance Show SemanticError where
  show (SemanticError pos msg) = show pos ++ ": " ++ msg
    