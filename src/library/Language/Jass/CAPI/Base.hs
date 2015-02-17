{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Jass.CAPI.Base(
    saveError
  , saveZeroBasedError
  , setLastError
  , saveFuncPtrBasedError
  ) where
  
import Foreign.C.String
import Foreign.C.Types
import Data.IORef
import Data.Global
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Hashable
import Foreign.Ptr

lastErrorVar :: IORef String
lastErrorVar = declareIORef "lastErrorVar" ""

setLastError :: MonadIO m => String -> m ()
setLastError msg = liftIO $ writeIORef lastErrorVar msg

saveError :: (MonadIO m, Show e) => ExceptT e m a -> m (Maybe a)
saveError action = do
  res <- runExceptT action
  case res of
    Left err -> liftIO $ writeIORef lastErrorVar (show err) >> return Nothing
    Right val -> return $ Just val

saveZeroBasedError :: (MonadIO m, Num a, Show e) => ExceptT e m a -> m a
saveZeroBasedError action = do
  res <- runExceptT action
  case res of
    Left err -> liftIO $ writeIORef lastErrorVar (show err) >> return 0
    Right val -> return val

saveFuncPtrBasedError :: (MonadIO m, Show e) => ExceptT e m (FunPtr a) -> m (FunPtr a)
saveFuncPtrBasedError action = do
  res <- runExceptT action
  case res of
    Left err -> liftIO $ do
      writeIORef lastErrorVar (show err) 
      return $ castPtrToFunPtr nullPtr
    Right val -> return val
        
c_getLastError :: IO CString
c_getLastError = newCString =<< readIORef lastErrorVar

foreign export ccall "hjassGetLastError" c_getLastError :: IO CString

instance Hashable CInt where
  hashWithSalt s (CInt i) = hashWithSalt s i