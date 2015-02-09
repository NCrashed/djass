module Language.Jass.JIT.Module(
    ExtractAST(..)
  , ParsedModule(..)
  , UnlinkedModule(..)
  , JITModule(..)
  , nativesMapFromMapping
  , isAllNativesBinded
  , nativesMapBind
  , checkNativesName
  , getNativesBindings
  ) where

import Language.Jass.Codegen.Context
import Language.Jass.Codegen.Type
import LLVM.General.AST as LLVMAST
import LLVM.General.Module as LLVM
import LLVM.General.ExecutionEngine
import Control.Monad
import Control.Monad.Trans.Except
import Foreign.Ptr
import Data.HashMap.Strict as HM
import Data.Maybe
import Data.Either
import Data.List (nub)

-- | Compiled module with unset natives
data ParsedModule = ParsedModule NativesMapping TypesMap LLVMAST.Module
-- | Raised into llvm module
data UnlinkedModule = UnlinkedModule NativesMap TypesMap LLVM.Module
-- | Executing module
data JITModule = JITModule TypesMap (ExecutableModule JIT)

type NativesMap = HashMap String (Either LLVMAST.Name (LLVMAST.Name, FunPtr ()))

class ExtractAST a where
  extractAST :: a -> IO LLVMAST.Module

instance ExtractAST ParsedModule where
  extractAST (ParsedModule _ _ m) = return m
  
instance ExtractAST UnlinkedModule where
  extractAST (UnlinkedModule _ _ m) = moduleAST m
 
-- | Creates new native map from mapping (user should fill all natives with ptrs)
nativesMapFromMapping :: NativesMapping -> NativesMap
nativesMapFromMapping = HM.map Left

-- | Returns first native that wasn't set
isAllNativesBinded :: NativesMap -> Maybe String
isAllNativesBinded = HM.foldlWithKey' isSet Nothing
  where isSet Nothing key (Left _) = Just key
        isSet Nothing _ (Right _) = Nothing
        isSet a@(Just _) _ _ = a

-- | Binds one native to function pointer
nativesMapBind :: String -> FunPtr a -> NativesMap -> NativesMap
nativesMapBind nativeName ptr mapping = HM.insert nativeName (Right (llvmName, castFunPtr ptr)) mapping 
  where (Left llvmName) = fromJust $ HM.lookup nativeName mapping

-- | Check sanity of user input
checkNativesName :: [String] -> NativesMap -> ExceptT String IO ()
checkNativesName names mapping 
  | length (nub names) /= length names = throwE "Natives bindings has duplicates!"
  | otherwise = forM_ names $ \name -> 
    case HM.lookup name mapping of
      Nothing -> throwE $ "Native '" ++ name ++ "' cannot be found!"
      Just (Right _) -> throwE $ "Native '" ++ name ++ "' is already binded!"
      _ -> return ()

-- | Returns only prepared bindings
getNativesBindings :: NativesMap -> [(LLVMAST.Name, FunPtr ())]
getNativesBindings = rights . HM.elems