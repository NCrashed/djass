module Language.Jass.JIT.Executing(
    loadJassModule
  , loadJassModuleFromFile
  , withRaisedAST
  , optimizeModule
  , moduleAssembly
  , withJassJIT
  , NativeTableMaker
  ) where

import Language.Jass.Runtime.Memory
import Language.Jass.Runtime.Natives
import Language.Jass.Runtime.Globals
import Language.Jass.Utils
import Language.Jass.Codegen.Generator
import Language.Jass.Semantic.Check
import Language.Jass.Parser.Grammar
import Language.Jass.JIT.Module
import LLVM.General.Module as LLVM
import LLVM.General.ExecutionEngine
import LLVM.General.PassManager
import LLVM.General.Context
import Control.Monad
import Control.Monad.Trans.Except
import Control.Applicative
import Foreign.Ptr
import Control.Monad.IO.Class (liftIO)

-- | Users defines this function to specify natives
type NativeTableMaker = JITModule -> ExceptT String IO [(String, FunPtr ())]

loadJassModule :: String -> String -> ExceptT String IO ParsedModule
loadJassModule name code = loadJassFromSource name $ liftExceptPure $ parseJass name code

loadJassModuleFromFile :: FilePath -> ExceptT String IO ParsedModule
loadJassModuleFromFile path = loadJassFromSource path $ liftExcept $ parseJassFile path

loadJassFromSource :: String -> ExceptT String IO JassModule -> ExceptT String IO ParsedModule
loadJassFromSource modName source = do
  tree <- source
  context <- liftExceptPure $ checkModuleSemantic tree
  triple <- liftExceptPure $ uncurry3 (generateLLVM modName) context
  return $ uncurry3 ParsedModule triple

withRaisedAST :: Context -> ParsedModule -> (UnlinkedModule -> ExceptT String IO a) -> ExceptT String IO a
withRaisedAST cntx (ParsedModule mapping tmap module') f = do
  let map' = nativesMapFromMapping mapping
  res <- withModuleFromAST cntx module' $ \mod' -> runExceptT $ f $ UnlinkedModule map' tmap mod'
  liftExceptPure res

moduleAssembly :: UnlinkedModule -> ExceptT String IO String
moduleAssembly (UnlinkedModule _ _ llvmModule) = liftIO $ moduleLLVMAssembly llvmModule

optimizeModule :: UnlinkedModule -> ExceptT String IO ()
optimizeModule (UnlinkedModule _ _ llvmModule) = liftIO $ void $ withPassManager set $ \ mng -> runPassManager mng llvmModule
  where set = defaultCuratedPassSetSpec {
                optLevel = Just 3
              , simplifyLibCalls = Just True
              , loopVectorize = Just True
              , superwordLevelParallelismVectorize = Just True
              , useInlinerWithThreshold = Just 1000
              }
  
withJassJIT :: Context -> NativeTableMaker -> UnlinkedModule -> (JITModule -> ExceptT String IO a) -> ExceptT String IO a
withJassJIT cntx nativesMaker (UnlinkedModule nativesMap tmap llvmModule) action = 
  liftExcept $ withJIT cntx 3 $ \jit -> withModuleInEngine jit llvmModule $ \exModule -> runExceptT $ do
    let jitModule = JITModule tmap exModule
    natives <- nativesMaker jitModule
    checkNativesName (fst <$> natives) nativesMap
    let bindedNatives = foldl (\mp f -> f mp) nativesMap $ fmap (uncurry nativesMapBind) natives
    case isAllNativesBinded bindedNatives of
        Just name -> throwE $ "Native '" ++ name ++ "' isn't binded!"
        Nothing -> do
          mapM_ (uncurry $ callNativeBinder jitModule) $ getNativesBindings bindedNatives
          setDefaultAllocator jitModule
          executeGlobalInitializers jitModule
          action jitModule