module Language.Jass.JIT.Module(
    loadJassModule
  , loadJassModuleFromFile
  , optimizeModule
  , executeMain
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

mapLeft ::  (e -> e') -> Either e a -> Either e' a
mapLeft f me = case me of
    Left err -> Left $ f err
    Right r -> Right r
    
liftExcept :: (Functor m, Monad m, Show e) => m (Either e a) -> ExceptT String m a
liftExcept action = ExceptT $ mapLeft show <$> action

liftExceptPure :: (Functor m, Monad m, Show e) => Either e a -> ExceptT String m a
liftExceptPure action = ExceptT $ return $ mapLeft show action

loadJassModule :: String -> String -> ExceptT String IO LLVMAST.Module
loadJassModule name code = loadJassFromSource $ liftExceptPure $ parseJass name code

loadJassModuleFromFile :: FilePath -> ExceptT String IO LLVMAST.Module
loadJassModuleFromFile path = loadJassFromSource $ liftExcept $ parseJassFile path

loadJassFromSource :: ExceptT String IO JassModule -> ExceptT String IO LLVMAST.Module
loadJassFromSource source = do
  tree <- source
  context <- liftExceptPure $ checkModuleSemantic tree
  liftExceptPure $ uncurry3 generateLLVM context
  
optimizeModule :: LLVM.Module -> IO ()
optimizeModule llvmModule = void $ withPassManager set $ \ mng -> runPassManager mng llvmModule
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
  
executeMain :: Context -> LLVM.Module -> IO ()
executeMain cntx llvmModule = withJIT cntx 3 $ \jit -> withModuleInEngine jit llvmModule $ \exModule -> do
  mptr <- getFunction exModule (Name "main")
  case mptr of
    Nothing -> fail "Cannot find main in jass module!"
    Just ptr -> do
      let mainFunc = mkJassMain $ castFunPtr ptr
      mainFunc
  
   
{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c