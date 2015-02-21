{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Jass.CAPI.Base(
    saveError
  , saveZeroBasedError
  , setLastError
  , saveFuncPtrBasedError
  ) where
  
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Data.IORef
import Data.Global
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Hashable
import Foreign.Ptr
import System.IO.Unsafe 

lastErrorVar :: IORef CString
lastErrorVar = declareIORef "lastErrorVar" $ unsafePerformIO $ newCString ""

setLastError :: MonadIO m => String -> m ()
setLastError msg = liftIO $ do
  free =<< readIORef lastErrorVar
  writeIORef lastErrorVar =<< newCString msg

saveError :: (MonadIO m, Show e) => ExceptT e m a -> m (Maybe a)
saveError action = do
  res <- runExceptT action
  case res of
    Left err -> liftIO $ setLastError (show err) >> return Nothing
    Right val -> return $ Just val

saveZeroBasedError :: (MonadIO m, Num a, Show e) => ExceptT e m a -> m a
saveZeroBasedError action = do
  res <- runExceptT action
  case res of
    Left err -> liftIO $ setLastError (show err) >> return 0
    Right val -> return val

saveFuncPtrBasedError :: (MonadIO m, Show e) => ExceptT e m (FunPtr a) -> m (FunPtr a)
saveFuncPtrBasedError action = do
  res <- runExceptT action
  case res of
    Left err -> liftIO $ do
      setLastError (show err) 
      return $ castPtrToFunPtr nullPtr
    Right val -> return val
        
cGetLastError :: IO CString
cGetLastError = readIORef lastErrorVar

foreign export ccall "hjassGetLastError" cGetLastError :: IO CString

instance Hashable CInt where
  hashWithSalt s (CInt i) = hashWithSalt s i