module Language.Jass.CAPI.Runtime.Code(

  ) where

import Language.Jass.CAPI.Base
import Language.Jass.CAPI.JassType
import Language.Jass.CAPI.JIT.Module
import Language.Jass.Runtime.Code
import Language.Jass.JIT.Module
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Global
import Data.IORef
import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class

type CJassCode = CInt

jassCodeTable :: IORef (CInt, HM.HashMap CJassCode JassCode)
jassCodeTable = declareIORef "jassCodeTable" (1, HM.empty)

insertIntoJassCodeTable :: MonadIO m => JassCode -> m CJassCode
insertIntoJassCodeTable prog = do
  (i, tbl) <- liftIO $ readIORef jassCodeTable
  liftIO $ writeIORef jassCodeTable (i+1, HM.insert i prog tbl)
  return i
  
lookupJassCodeTable :: MonadIO m => CJassCode -> m (Maybe JassCode)
lookupJassCodeTable cprog = do
  (_, tbl) <- liftIO $ readIORef jassCodeTable
  return $ HM.lookup cprog tbl

freeJassCodeTable :: MonadIO m => CJassCode -> m ()
freeJassCodeTable cprog = do
  (i, tbl) <- liftIO $ readIORef jassCodeTable
  liftIO $ writeIORef jassCodeTable (i, HM.delete cprog tbl)

withCodeRef :: (Num a, MonadIO m) => CJassCode -> (JassCode -> m a) -> m a
withCodeRef cjcode action = do
  mjcode <- lookupJassCodeTable cjcode
  case mjcode of
    Nothing -> setLastError "invalid code reference" >> return 0
    Just jcode -> action  jcode
    
-- | Reads inner structure of jass code reference
cLiftCode :: CJITModule -- ^ Identifier of jit module
  -> Ptr () -- ^ Pointer to jcode value
  -> IO CJassCode -- ^ zero value is error, you can get error message from hjassGetLastError
cLiftCode cjit ptr = withJITModuleRef cjit $ \jit -> saveZeroBasedError $ do
      jcode <- liftJassCode jit ptr
      insertIntoJassCodeTable jcode

foreign export ccall "hjassLiftCode" cLiftCode :: CJITModule -> Ptr () -> IO CJassCode 

-- | Frees jass code reference
cFreeCode :: CJassCode -> IO ()
cFreeCode = freeJassCodeTable

foreign export ccall "hjassFreeCode" cFreeCode :: CJassCode -> IO ()

-- | Returns function ptr from code value  
cCodeFunctionPtr :: CJassCode -- ^ Identifier of jass code value 
  -> Ptr (FunPtr ()) -- ^ where to store result 
  -> IO CInt -- ^ zero value is error, you can get error message from hjassGetLastError
cCodeFunctionPtr cjcode retptr = withCodeRef cjcode $ \jcode -> do
      poke retptr $ codeFunctionPtr jcode
      return 1
      
foreign export ccall "hjassCodeFunctionPtr" cCodeFunctionPtr :: CJassCode -> Ptr (FunPtr ()) -> IO CInt

-- | Returns result type of callback
cCodeReturnType :: CJITModule -- ^ Identifier of jit module 
  -> CJassCode -- ^ Identifier of jass code value
  -> Ptr CJassType -- ^ where to store result
  -> IO CInt -- ^ zero value is error, you can get error message from hjassGetLastError
cCodeReturnType cjit cjcode retptr = withJITModuleRef cjit $ \(JITModule (_,typeMap) _) -> withCodeRef cjcode $ \jcode ->
  case codeReturnType jcode of
    Nothing -> poke retptr 0 >> return 1
    Just rt -> 
      case HM.lookup rt typeMap of
        Nothing -> setLastError ("ICE: unknown type " ++ show rt) >> return 0
        Just tid -> poke retptr (fromIntegral tid) >> return 1

foreign export ccall "hjassCodeReturnType" cCodeReturnType :: CJITModule -> CJassCode -> Ptr CJassType -> IO CInt

-- | Returns arguments count of callback
cCodeArgumentCount :: CJassCode  -- ^ Identifier of jass code value
  -> Ptr CInt -- ^ where to store result
  -> IO CInt -- ^ zero value is error, you can get error message from hjassGetLastError
cCodeArgumentCount cjcode retptr = withCodeRef cjcode $ \jcode -> do
  poke retptr $ fromIntegral.length.codeArgumentTypes $ jcode
  return 1
  
foreign export ccall "hjassCodeArgumentCount" cCodeArgumentCount :: CJassCode -> Ptr CInt -> IO CInt
  
-- | Returns arguments count of callback
cCodeGetArgument :: CJITModule -- ^ Identifier of jit module  
  -> CJassCode  -- ^ Identifier of jass code value
  -> CInt -- ^ index of argument, zero based
  -> Ptr CJassType -- ^ where to store result
  -> IO CInt -- ^ zero value is error, you can get error message from hjassGetLastError
cCodeGetArgument cjit cjcode cindex retptr = withJITModuleRef cjit $ \(JITModule (_,typeMap) _) -> withCodeRef cjcode $ \jcode -> do
  let index = fromIntegral cindex
  if index < 0 || index >= length (codeArgumentTypes jcode) 
  then setLastError "argument index overflow" >> return 0
  else do
    let tp = codeArgumentTypes jcode !! index
    case HM.lookup tp typeMap of
      Nothing -> setLastError ("ICE: unknown type " ++ show tp) >> return 0
      Just tid -> do
        poke retptr $ fromIntegral tid 
        return 1
  
foreign export ccall "hjassCodeGetArgument" cCodeGetArgument :: CJITModule -> CJassCode -> CInt -> Ptr CJassType -> IO CInt