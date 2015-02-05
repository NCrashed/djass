module Language.Jass.JIT.Module(
    loadJassModule
  , loadJassModuleFromFile
  , withRaisedAST
  , optimizeModule
  , executeMain
  , moduleAssembly
  , ExtractAST(..)
  , UnlinkedModule
  , executeJass0
  , executeJass1
  , executeJass2
  , executeJass3
  , executeJass4
  , executeJass5
  , executeJass6
  , executeJass7
  ) where

import Language.Jass.Utils
import Language.Jass.Codegen.Generator
import Language.Jass.Semantic.Check
import Language.Jass.Parser.Grammar
import LLVM.General.AST as LLVMAST
import LLVM.General.Module as LLVM
import LLVM.General.ExecutionEngine
import LLVM.General.PassManager
import LLVM.General.Context
import Control.Monad
import Control.Monad.Trans.Except
import Control.Applicative
import Foreign.Ptr
import Data.HashMap.Strict as HM
import Data.Maybe
import Data.Either
import Data.List (nub)
import Control.Monad.IO.Class (liftIO)

-- | Compiled module with unset natives
data ParsedModule = ParsedModule NativesMapping LLVMAST.Module
data UnlinkedModule = UnlinkedModule NativesMap LLVM.Module
type NativesMap = HashMap String (Either LLVMAST.Name (LLVMAST.Name, FunPtr ()))

class ExtractAST a where
  extractAST :: a -> IO LLVMAST.Module

instance ExtractAST ParsedModule where
  extractAST (ParsedModule _ m) = return m
  
instance ExtractAST UnlinkedModule where
  extractAST (UnlinkedModule _ m) = moduleAST m
 
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

loadJassModule :: String -> String -> ExceptT String IO ParsedModule
loadJassModule name code = loadJassFromSource $ liftExceptPure $ parseJass name code

loadJassModuleFromFile :: FilePath -> ExceptT String IO ParsedModule
loadJassModuleFromFile path = loadJassFromSource $ liftExcept $ parseJassFile path

loadJassFromSource :: ExceptT String IO JassModule -> ExceptT String IO ParsedModule
loadJassFromSource source = do
  tree <- source
  context <- liftExceptPure $ checkModuleSemantic tree
  (mapping, module') <- liftExceptPure $ uncurry3 generateLLVM context
  return $ ParsedModule mapping module'

withRaisedAST :: Context -> ParsedModule -> (UnlinkedModule -> ExceptT String IO a) -> ExceptT String IO a
withRaisedAST cntx (ParsedModule mapping module') f = do
  let map' = nativesMapFromMapping mapping
  res <- withModuleFromAST cntx module' $ \mod' -> runExceptT $ f $ UnlinkedModule map' mod'
  liftExceptPure res

moduleAssembly :: UnlinkedModule -> ExceptT String IO String
moduleAssembly (UnlinkedModule _ llvmModule) = liftIO $ moduleLLVMAssembly llvmModule

optimizeModule :: UnlinkedModule -> ExceptT String IO ()
optimizeModule (UnlinkedModule _ llvmModule) = liftIO $ void $ withPassManager set $ \ mng -> runPassManager mng llvmModule
  where set = defaultCuratedPassSetSpec {
                optLevel = Just 3
              , simplifyLibCalls = Just True
              , loopVectorize = Just True
              , superwordLevelParallelismVectorize = Just True
              , useInlinerWithThreshold = Just 1000
              }
              
type JassMain = IO ()
foreign import ccall "dynamic"
  mkJassMain :: FunPtr JassMain -> JassMain  
  
executeMain :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> ExceptT String IO ()
executeMain cntx natives module' = executeJass0 cntx natives module' "main" mkJassMain

executeJass0 :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> String -> (FunPtr (IO a)-> IO a) -> ExceptT String IO a
executeJass1 :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> String -> (FunPtr (a -> IO b) -> a -> IO b) -> a -> ExceptT String IO b
executeJass2 :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> String -> (FunPtr (a -> b -> IO c) -> a -> b -> IO c) -> a -> b -> ExceptT String IO c
executeJass3 :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> String -> (FunPtr (a -> b -> c -> IO d) -> a -> b -> c -> IO d) -> a -> b -> c -> ExceptT String IO d
executeJass4 :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> String -> (FunPtr (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> ExceptT String IO e
executeJass5 :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> String -> (FunPtr (a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> ExceptT String IO f
executeJass6 :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> String -> (FunPtr (a -> b -> c -> d -> e -> f -> IO g) -> a -> b -> c -> d -> e -> f -> IO g) -> a -> b -> c -> d -> e -> f -> ExceptT String IO g
executeJass7 :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> String -> (FunPtr (a -> b -> c -> d -> e -> f -> g -> IO i) -> a -> b -> c -> d -> e -> f -> g -> IO i) -> a -> b -> c -> d -> e -> f -> g -> ExceptT String IO i

executeJass0 cntx natives module' funcName funcMaker = executeJass cntx natives module' $ \ex -> callFunc0 ex funcName funcMaker  
executeJass1 cntx natives module' funcName funcMaker arg1 = executeJass cntx natives module' $ \ex -> callFunc1 ex funcName funcMaker arg1
executeJass2 cntx natives module' funcName funcMaker arg1 arg2 = executeJass cntx natives module' $ \ex -> callFunc2 ex funcName funcMaker arg1 arg2
executeJass3 cntx natives module' funcName funcMaker arg1 arg2 arg3 = executeJass cntx natives module' $ \ex -> callFunc3 ex funcName funcMaker arg1 arg2 arg3 
executeJass4 cntx natives module' funcName funcMaker arg1 arg2 arg3 arg4 = executeJass cntx natives module' $ \ex -> callFunc4 ex funcName funcMaker arg1 arg2 arg3 arg4
executeJass5 cntx natives module' funcName funcMaker arg1 arg2 arg3 arg4 arg5 = executeJass cntx natives module' $ \ex -> callFunc5 ex funcName funcMaker arg1 arg2 arg3 arg4 arg5
executeJass6 cntx natives module' funcName funcMaker arg1 arg2 arg3 arg4 arg5 arg6 = executeJass cntx natives module' $ \ex -> callFunc6 ex funcName funcMaker arg1 arg2 arg3 arg4 arg5 arg6
executeJass7 cntx natives module' funcName funcMaker arg1 arg2 arg3 arg4 arg5 arg6 arg7 = executeJass cntx natives module' $ \ex -> callFunc7 ex funcName funcMaker arg1 arg2 arg3 arg4 arg5 arg6 arg7
            
executeJass :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> (ExecutableModule JIT -> ExceptT String IO a) -> ExceptT String IO a 
executeJass cntx natives (UnlinkedModule nativesMap llvmModule) action = do
  checkNativesName (fst <$> natives) nativesMap
  let bindedNatives = foldl (\mp f -> f mp) nativesMap $ fmap (uncurry nativesMapBind) natives
  case isAllNativesBinded bindedNatives of
      Just name -> throwE $ "Native '" ++ name ++ "' isn't binded!"
      Nothing -> liftExcept $ withJIT cntx 3 $ \jit -> withModuleInEngine jit llvmModule $ \exModule -> runExceptT $ do
                  mapM_ (uncurry $ callNativeBinder exModule) $ getNativesBindings bindedNatives
                  action exModule

callFunc0 :: ExecutableModule JIT -> String -> (FunPtr (IO a) -> IO a) -> ExceptT String IO a
callFunc1 :: ExecutableModule JIT -> String -> (FunPtr (a -> IO b) -> a -> IO b) -> a -> ExceptT String IO b
callFunc2 :: ExecutableModule JIT -> String -> (FunPtr (a -> b -> IO c) -> a -> b -> IO c) -> a -> b -> ExceptT String IO c
callFunc3 :: ExecutableModule JIT -> String -> (FunPtr (a -> b -> c -> IO d) -> a -> b -> c -> IO d) -> a -> b -> c -> ExceptT String IO d
callFunc4 :: ExecutableModule JIT -> String -> (FunPtr (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> ExceptT String IO e
callFunc5 :: ExecutableModule JIT -> String -> (FunPtr (a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> ExceptT String IO f
callFunc6 :: ExecutableModule JIT -> String -> (FunPtr (a -> b -> c -> d -> e -> f -> IO g) -> a -> b -> c -> d -> e -> f -> IO g) -> a -> b -> c -> d -> e -> f -> ExceptT String IO g
callFunc7 :: ExecutableModule JIT -> String -> (FunPtr (a -> b -> c -> d -> e -> f -> g -> IO i) -> a -> b -> c -> d -> e -> f -> g -> IO i) -> a -> b -> c -> d -> e -> f -> g -> ExceptT String IO i

callFunc0 exModule funcName funcMaker = callFunc exModule funcName $ \ptr -> liftIO $ funcMaker $ castFunPtr ptr
callFunc1 exModule funcName funcMaker arg1 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1
callFunc2 exModule funcName funcMaker arg1 arg2 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2
callFunc3 exModule funcName funcMaker arg1 arg2 arg3 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3
callFunc4 exModule funcName funcMaker arg1 arg2 arg3 arg4 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3 arg4
callFunc5 exModule funcName funcMaker arg1 arg2 arg3 arg4 arg5 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3 arg4 arg5
callFunc6 exModule funcName funcMaker arg1 arg2 arg3 arg4 arg5 arg6 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3 arg4 arg5 arg6
callFunc7 exModule funcName funcMaker arg1 arg2 arg3 arg4 arg5 arg6 arg7 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3 arg4 arg5 arg6 arg7

callFunc :: ExecutableModule JIT -> String -> (FunPtr () -> ExceptT String IO a) -> ExceptT String IO a 
callFunc exModule funcName action = do
  mptr <- liftIO $ getFunction exModule (Name funcName)
  case mptr of
    Nothing -> throwE "Cannot find main in jass module!"
    Just ptr -> action ptr
    
type NativeBinder = FunPtr () -> IO ()
foreign import ccall "dynamic"
  mkNativeBinder :: FunPtr NativeBinder -> NativeBinder
  
callNativeBinder :: ExecutableModule JIT -> LLVMAST.Name -> FunPtr () -> ExceptT String IO ()
callNativeBinder ex nativeName ptr = do
  mptr <- liftIO $ getFunction ex nativeName
  case mptr of
    Nothing -> throwE $ "ICE: invalid name of native binder " ++ show nativeName
    Just binderPtr -> do
      let binder = mkNativeBinder $ castFunPtr binderPtr
      liftIO $ binder ptr