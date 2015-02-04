module Language.Jass.Semantic.SemanticError(
  -- | Error handling
  SemanticError(..),
  updateErrorMsg
  ) where
  
import Language.Jass.Parser.AST
import Control.Monad.Error

-- | Semantic error holds source position and a message
data SemanticError = SemanticError SrcPos String 

-- | Updating error message without changing source position
updateErrorMsg :: SemanticError -> (SrcPos -> String) -> SemanticError
updateErrorMsg (SemanticError pos _) msgFunc = SemanticError pos $ msgFunc pos

-- | Creating error without source position
strMsg_ :: String -> SemanticError
strMsg_ = SemanticError noPos

-- | Empty error
noMsg_ :: SemanticError
noMsg_ = strMsg_ ""

instance Show SemanticError where
  show (SemanticError pos msg) = show pos ++ ": " ++ msg

instance Error SemanticError where
  noMsg = noMsg_
  strMsg = strMsg_ 