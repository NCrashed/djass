module Language.Jass.Codegen.CodegenError(
  CodegenError,
  updateErrorMsg,
  showPos
  ) where
  
import Language.Jass.Parser.AST
import Control.Monad.Error

-- | Code generation error holds source (sometimes not) position and a message
data CodegenError = CodegenError (Maybe SrcPos) String 

-- | Updating error message without changing source position
updateErrorMsg :: CodegenError -> (SrcPos -> String) -> CodegenError
updateErrorMsg (CodegenError mp@(Just pos) _) msgFunc = CodegenError mp $ msgFunc pos
updateErrorMsg err _ = err

-- | Pretty printing of source position
showPos :: SrcPos -> String
showPos src = locFile src ++ "( line " ++ show (locLine src) ++ ", column " ++ show (locCol src) ++ ")"

instance Error CodegenError where
  noMsg = strMsg ""
  strMsg = CodegenError Nothing
  
instance Show CodegenError where
  show (CodegenError (Just pos) msg) = showPos pos ++ ": " ++ msg
  show (CodegenError Nothing msg) = msg
    