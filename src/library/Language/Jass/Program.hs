module Language.Jass.Program(
    makeEmptyNativeTable
  , JassProgram
  , JITModule
  , NativeTableMaker
  , constructProgram
  , constructProgramFromSource
  , executeProgram
  , module ReExport
  ) where

import qualified Data.HashMap.Strict as HM
import Language.Jass.JIT.Executing
import Language.Jass.JIT.Module
import Language.Jass.JIT.Calling as ReExport
import Language.Jass.Parser.Grammar
import Language.Jass.Semantic.Check
import Language.Jass.Codegen.Generator
import Language.Jass.Utils
import LLVM.General.Context
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import System.Directory
import System.FilePath
import qualified Data.Traversable as T
import Filesystem (isFile)
import Data.String

-- | Helper to generate programs without natives
makeEmptyNativeTable :: NativeTableMaker
makeEmptyNativeTable _ = return []

-- | Creates jass program from set of modules
constructProgram :: 
  HM.HashMap String String -- ^ List of (module name, source code)
  -> String -- ^ Name of main module
  -> ExceptT String IO JassProgram -- ^ Parsed and checked jass program
constructProgram imports mainModuleName = do
  parsedImports <- T.sequence $ HM.mapWithKey (\k v -> liftExceptPure $ parseJass k v) imports
  case HM.lookup mainModuleName parsedImports of
    Nothing -> throwE $ "Cannot find main module with name: " ++ mainModuleName
    Just mainModule -> do
      context <- liftExceptPure $ checkModuleSemantic parsedImports mainModule
      uncurry3 JassProgram <$> liftExceptPure (uncurry3 (generateLLVM mainModuleName) context)
      
-- | Creates jass program from set of modules stored in filesystem
constructProgramFromSource ::
  FilePath -- ^ Path to folder
  -> FilePath -- ^ Relative path to main module
  -> ExceptT String IO JassProgram -- ^ Parsed and checked jass program
constructProgramFromSource folder mainPath = do
  isMainExists <- liftIO $ doesFileExist fullMainPath
  unless isMainExists $ throwE $ "Cannot find main module at " ++ fullMainPath
  fileList <- liftIO $ filterJassFiles folder =<< getDirectoryContents folder
  fileSources <- liftIO $ mapM readFile $ fmap (\p -> folder ++ "/" ++ p) fileList
  let modulesMap = HM.fromList $ fmap makeModuleName fileList `zip` fileSources 
  constructProgram modulesMap $ makeModuleName mainPath
  where
    fullMainPath = folder ++ "/" ++ mainPath

executeProgram ::
  JassProgram -- ^ Loaded jass program
  -> NativeTableMaker -- ^ User natives
  -> (JITModule -> ExceptT String IO a) -- ^ What to do while executing
  -> ExceptT String IO a -- ^ Result of execution
executeProgram prog natives action = liftExcept $ withContext $ \cntx -> runExceptT $ 
  withRaisedAST cntx prog $ \llvmModule -> do
    optimizeModule llvmModule
    withJassJIT cntx natives llvmModule action

-- | some/path/to/module.j => some.path.to.module
makeModuleName :: String -> String
makeModuleName s = map replDots $ dropExtension s
  where
  replDots '/' = '.'
  replDots '\\' = '.'
  replDots c = c
  
-- | Removes non .j files
filterJassFiles :: FilePath -> [FilePath] -> IO [FilePath]
filterJassFiles folder = filterM cond
  where 
  cond nm =  do
    isf <- isFile $ fromString $ folder ++ "/" ++ nm
    return $ isf && takeExtension nm == ".j"