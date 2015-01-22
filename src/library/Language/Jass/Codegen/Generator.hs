module Language.Jass.Codegen.Generator(
  generateLLVM
  ) where
  
import Language.Jass.Codegen.Context
import Language.Jass.Codegen.CodegenError
import Language.Jass.Parser.AST.TypeDef
import Language.Jass.Semantic.Callable
import Language.Jass.Semantic.Variable
import LLVM.General.AST
import Control.Monad

generateLLVM :: [TypeDef] -> [Callable] -> [Variable] -> Either CodegenError Module
generateLLVM types callables variables = runCodegen context $ do
  mapM_ (addDefinition <=< genLLVM) types
  mapM_ (addDefinition <=< genLLVM) callables
  mapM_ (addDefinition <=< genLLVM) variables
  getModule
  where context = newContext types callables variables
        
class LLVMDefinition a where
  genLLVM :: a -> Codegen Definition  

instance LLVMDefinition TypeDef where
  genLLVM _ = undefined
  
instance LLVMDefinition Variable where
  genLLVM _ = undefined
  
instance LLVMDefinition Callable where
  genLLVM _ = undefined