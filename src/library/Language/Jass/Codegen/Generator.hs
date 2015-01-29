module Language.Jass.Codegen.Generator(
  generateLLVM
  ) where
  
import Language.Jass.Codegen.Context
import Language.Jass.Codegen.CodegenError
import Language.Jass.Parser.AST.TypeDef
import Language.Jass.JassType
import Language.Jass.Semantic.Callable
import Language.Jass.Semantic.Variable
import LLVM.General.AST
import LLVM.General.AST.Type
import Control.Monad

generateLLVM :: [TypeDef] -> [Callable] -> [Variable] -> Either CodegenError Module
generateLLVM types callables variables = runCodegen context $ do
  genBasicTypesDefinition
  mapM_ (addDefinition <=< genLLVM) types
  mapM_ (addDefinition <=< genLLVM) callables
  mapM_ (addDefinition <=< genLLVM) variables
  getModule
  where context = newContext types callables variables

genBasicTypesDefinition :: Codegen ()
genBasicTypesDefinition = undefined

getBasicTypeName :: JassType -> Name
getBasicTypeName JInteger = Name "integer"
getBasicTypeName JReal = Name "real"
getBasicTypeName JBoolean = Name "boolean"
getBasicTypeName JString = Name "string"
getBasicTypeName JHandle = Name "handle"
getBasicTypeName JCode = Name "code"
--getBasicTypeName _ 

getBasicTypeDefinition :: JassType -> Definition
getBasicTypeDefinition JInteger = TypeDefinition (Name "integer") $ Just i32
getBasicTypeDefinition JReal = TypeDefinition (Name "real") $ Just float
getBasicTypeDefinition JBoolean = TypeDefinition (Name "boolean") $ Just i1
getBasicTypeDefinition JString = TypeDefinition (Name "string") $ Just (ptr i8)

class LLVMDefinition a where
  genLLVM :: a -> Codegen Definition  

instance LLVMDefinition TypeDef where
 -- genLLVM (TypeDef _ name JInteger) = return $ 
 -- genLLVM (TypeDef _ name J
  
instance LLVMDefinition Variable where
  genLLVM _ = undefined
  
instance LLVMDefinition Callable where
  genLLVM _ = undefined