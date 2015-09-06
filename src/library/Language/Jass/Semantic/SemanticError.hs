module Language.Jass.Semantic.SemanticError(
  -- | Error handling
    SemanticError(..)
  , updateErrorMsg
  , unplacedSemError
  , noMsg
  ) where
  
import Language.Jass.Parser.AST

-- | Semantic error holds source position and a message
data SemanticError = SemanticError SrcPos String 

-- | Updating error message without changing source position
updateErrorMsg :: SemanticError -> (SrcPos -> String) -> SemanticError
updateErrorMsg (SemanticError pos _) msgFunc = SemanticError pos $ msgFunc pos

instance Show SemanticError where
  show (SemanticError pos msg) = show pos ++ ": " ++ msg

-- | Semantic error that isn't binded to specific source pos
unplacedSemError :: String -> SemanticError
unplacedSemError s = SemanticError noPos s 

-- | Empty semantic error
noMsg :: SemanticError
noMsg = SemanticError noPos ""