module Language.Jass.JIT.Executing(
    loadJassModule
  , loadJassModuleFromFile
  , withRaisedAST
  , optimizeModule
  , moduleAssembly
  , withJassJIT
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
  
withJassJIT :: Context -> [(String, FunPtr ())] -> UnlinkedModule -> (JITModule -> ExceptT String IO a) -> ExceptT String IO a
withJassJIT cntx natives (UnlinkedModule nativesMap llvmModule) action = do
  checkNativesName (fst <$> natives) nativesMap
  let bindedNatives = foldl (\mp f -> f mp) nativesMap $ fmap (uncurry nativesMapBind) natives
  case isAllNativesBinded bindedNatives of
      Just name -> throwE $ "Native '" ++ name ++ "' isn't binded!"
      Nothing -> liftExcept $ withJIT cntx 3 $ \jit -> withModuleInEngine jit llvmModule $ \exModule -> runExceptT $ do
                    let jitModule = JITModule exModule
                    mapM_ (uncurry $ callNativeBinder jitModule) $ getNativesBindings bindedNatives
                    setDefaultAllocator jitModule
                    executeGlobalInitializers jitModule
                    action jitModule