module Language.Jass.Semantic.SemanticError(
  -- | Error handling
  SemanticError(..),
  updateErrorMsg,
  ) where
  
import Language.Jass.Parser.AST
import Control.Monad.Error

-- | Semantic error holds source position and a message
data SemanticError = SemanticError SrcPos String 

-- | Updating error message without changing source position
updateErrorMsg :: SemanticError -> (SrcPos -> String) -> SemanticError
updateErrorMsg (SemanticError pos _) msgFunc = SemanticError pos $ msgFunc pos
 
instance Error SemanticError where
  noMsg = strMsg ""
  strMsg = SemanticError (SrcPos "" 0 0 0)