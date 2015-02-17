module Language.Jass.CAPI.JIT.Module(
    CProgram
  , insertIntoProgramTable
  , lookupProgramTable
  , freeProgramTable
  , CJITModule
  , insertIntoJITModuleTable
  , lookupJITModuleTable
  , freeJITModuleTable
  ) where

import Foreign.C.Types
import Data.Global
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class

import Language.Jass.CAPI.Base()
import Language.Jass.JIT.Module

-- | Descriptor of a program
type CProgram = CInt

programTable :: IORef (CInt, HM.HashMap CProgram JassProgram)
programTable = declareIORef "programTable" (1, HM.empty)

insertIntoProgramTable :: MonadIO m => JassProgram -> m CProgram
insertIntoProgramTable prog = do
  (i, tbl) <- liftIO $ readIORef programTable
  liftIO $ writeIORef programTable (i+1, HM.insert i prog tbl)
  return i
  
lookupProgramTable :: MonadIO m => CProgram -> m (Maybe JassProgram)
lookupProgramTable cprog = do
  (_, tbl) <- liftIO $ readIORef programTable
  return $ HM.lookup cprog tbl

freeProgramTable :: MonadIO m => CProgram -> m ()
freeProgramTable cprog = do
  (i, tbl) <- liftIO $ readIORef programTable
  liftIO $ writeIORef programTable (i, HM.delete cprog tbl)
  
  -- | Descriptor of a executing module
type CJITModule = CInt

jitModuleTable :: IORef (CInt, HM.HashMap CJITModule JITModule)
jitModuleTable = declareIORef "jitModuleTable" (1, HM.empty)

insertIntoJITModuleTable :: MonadIO m => JITModule -> m CJITModule
insertIntoJITModuleTable module' = do
  (i, tbl) <- liftIO $ readIORef jitModuleTable
  liftIO $ writeIORef jitModuleTable (i+1, HM.insert i module' tbl)
  return i
  
lookupJITModuleTable :: MonadIO m => CJITModule -> m (Maybe JITModule)
lookupJITModuleTable cmodule = do
  (_, tbl) <- liftIO $ readIORef jitModuleTable
  return $ HM.lookup cmodule tbl
  
freeJITModuleTable :: MonadIO m => CJITModule -> m ()
freeJITModuleTable cmodule = do
  (i, tbl) <- liftIO $ readIORef jitModuleTable
  liftIO $ writeIORef jitModuleTable (i, HM.delete cmodule tbl)