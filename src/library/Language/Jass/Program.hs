module Language.Jass.Program(
    makeEmptyNativeTable
  , JassProgram(..)
  , constructProgram
  , constructProgramFromSource
  , executeProgram
  ) where

import qualified Data.HashMap.Strict as HM
import Language.Jass.JIT.Executing
import Language.Jass.JIT.Module
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import System.Directory
import System.FilePath

data JassProgram

-- | Helper to generate programs without natives
makeEmptyNativeTable :: NativeTableMaker
makeEmptyNativeTable _ = return []

-- | Creates jass program from set of modules
constructProgram :: 
  HM.HashMap String String -- ^ List of (module name, source code)
  -> String -- ^ Name of main module
  -> NativeTableMaker -- ^ User natives
  -> ExceptT String IO JassProgram -- ^ Parsed and checked jass program
constructProgram = undefined

-- | Creates jass program from set of modules stored in filesystem
constructProgramFromSource ::
  FilePath -- ^ Path to folder
  -> FilePath -- ^ Relative path to main module
  -> NativeTableMaker -- ^ User natives
  -> ExceptT String IO JassProgram -- ^ Parsed and checked jass program
constructProgramFromSource folder mainPath nativeMaker = do
  isMainExists <- liftIO $ doesFileExist fullMainPath
  unless isMainExists $ throwE $ "Cannot find main module at " ++ fullMainPath
  fileList <- liftIO $ getDirectoryContents folder
  fileSources <- liftIO $ mapM readFile fileList
  let modulesMap = HM.fromList $ fmap makeModuleName fileList `zip` fileSources 
  constructProgram modulesMap (makeModuleName mainPath) nativeMaker
  where
    fullMainPath = folder ++ "/" ++ mainPath
    
executeProgram ::
  JassProgram -- ^ Loaded jass program
  -> (JITModule -> ExceptT String IO a) -- ^ What to do while executing
  -> ExceptT String IO a -- ^ Result of execution
executeProgram = undefined
 
-- | some/path/to/module.j => some.path.to.module
makeModuleName :: String -> String
makeModuleName s = map replDots $ dropExtension s
  where
  replDots '/' = '.'
  replDots '\\' = '.'
  replDots c = c