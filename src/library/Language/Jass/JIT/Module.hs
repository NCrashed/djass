module Language.Jass.JIT.Module(
    loadJassModule
  , loadJassModuleFromFile
  , withRaisedAST
  , optimizeModule
  , executeMain
  , moduleAssembly
  , extractAST
  , UnlinkedModule
  ) where

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

mapLeft ::  (e -> e') -> Either e a -> Either e' a
mapLeft f me = case me of
    Left err -> Left $ f err
    Right r -> Right r

-- | Compiled module with unset natives
data UnlinkedModule = UnlinkedModule NativesMap (Either LLVMAST.Module LLVM.Module)
type NativesMap = HashMap String (Either LLVMAST.Name (LLVMAST.Name, FunPtr ()))

extractAST :: UnlinkedModule -> IO LLVMAST.Module
extractAST (UnlinkedModule _ (Left m)) = return m
extractAST (UnlinkedModule _ (Right m)) = moduleAST m
 
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

liftExcept :: (Functor m, Monad m, Show e) => m (Either e a) -> ExceptT String m a
liftExcept action = ExceptT $ mapLeft show <$> action

liftExceptPure :: (Functor m, Monad m, Show e) => Either e a -> ExceptT String m a
liftExceptPure action = ExceptT $ return $ mapLeft show action

loadJassModule :: String -> String -> ExceptT String IO UnlinkedModule
loadJassModule name code = loadJassFromSource $ liftExceptPure $ parseJass name code

loadJassModuleFromFile :: FilePath -> ExceptT String IO UnlinkedModule
loadJassModuleFromFile path = loadJassFromSource $ liftExcept $ parseJassFile path

loadJassFromSource :: ExceptT String IO JassModule -> ExceptT String IO UnlinkedModule
loadJassFromSource source = do
  tree <- source
  context <- liftExceptPure $ checkModuleSemantic tree
  (mapping, module') <- liftExceptPure $ uncurry3 generateLLVM context
  return $ UnlinkedModule (nativesMapFromMapping mapping) (Left module')

withRaisedAST :: Context -> UnlinkedModule -> (UnlinkedModule -> ExceptT String IO a) -> ExceptT String IO a
withRaisedAST _ module'@(UnlinkedModule _ (Right _)) f = f module'
withRaisedAST cntx (UnlinkedModule m (Left module')) f = do
  res <- withModuleFromAST cntx module' $ \mod' -> runExceptT $ f $ UnlinkedModule m (Right mod')
  liftExceptPure res

moduleAssembly :: UnlinkedModule -> ExceptT String IO String
moduleAssembly (UnlinkedModule _ (Left _)) = throwE "Use withRaisedAST first"
moduleAssembly (UnlinkedModule _ (Right llvmModule)) = liftIO $ moduleLLVMAssembly llvmModule

optimizeModule :: UnlinkedModule -> ExceptT String IO ()
optimizeModule (UnlinkedModule _ (Left _)) = throwE "Use withRaisedAST first"
optimizeModule (UnlinkedModule _ (Right llvmModule)) = liftIO $ void $ withPassManager set $ \ mng -> runPassManager mng llvmModule
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
executeMain _ _ (UnlinkedModule _ (Left _)) = fail "Use withRaisedAST first"
executeMain cntx natives (UnlinkedModule nativesMap (Right llvmModule)) = do
  checkNativesName (fst <$> natives) nativesMap
  let bindedNatives = foldl (\mp f -> f mp) nativesMap $ fmap (uncurry nativesMapBind) natives
  case isAllNativesBinded bindedNatives of
      Just name -> throwE $ "Native '" ++ name ++ "' isn't binded!"
      Nothing -> liftExcept $ withJIT cntx 3 $ \jit -> withModuleInEngine jit llvmModule $ \exModule -> runExceptT $ do
                  mapM_ (uncurry $ callNativeBinder exModule) $ getNativesBindings bindedNatives
                  callMain exModule
  

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

callMain :: ExecutableModule JIT -> ExceptT String IO ()
callMain ex = do 
  mptr <- liftIO $ getFunction ex (Name "main")
  case mptr of
    Nothing -> throwE "Cannot find main in jass module!"
    Just ptr -> do
      let mainFunc = mkJassMain $ castFunPtr ptr
      liftIO mainFunc
      
{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c