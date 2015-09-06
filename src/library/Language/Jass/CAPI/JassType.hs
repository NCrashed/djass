module Language.Jass.CAPI.JassType(
    CJassType
  ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Language.Jass.JassType
import Language.Jass.JIT.Module
import Language.Jass.CAPI.JIT.Module
import Language.Jass.CAPI.Base
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

type CJassType = CInt

getTypeId :: JassType -> CJITModule -> Ptr CJassType -> IO CInt 
getTypeId jt cjit retptr = withJITModuleRef cjit $ \(JITModule (_,typeMap) _) ->
  case HM.lookup jt typeMap of
    Nothing -> setLastError ("Cannot find id for type " ++ show jt) >> return 0
    Just tid -> poke retptr (fromIntegral tid) >> return 1

getTypeFromId :: CJITModule -> CJassType -> ExceptT String IO JassType
getTypeFromId cjit ctp = withJITModuleRefE cjit $ \(JITModule (typeMap,_) _) ->
  case HM.lookup (fromIntegral ctp) typeMap of
    Nothing -> throwE $ "Cannot find type by id " ++ show cjit
    Just jt -> return jt
    
cVoid :: CJITModule -> Ptr CJassType -> IO CInt 
cVoid _ retptr = poke retptr 0 >> return 1
foreign export ccall "hjassVoid" cVoid :: CJITModule -> Ptr CJassType -> IO CInt 

cInteger :: CJITModule -> Ptr CJassType -> IO CInt 
cInteger = getTypeId JInteger
foreign export ccall "hjassInteger" cInteger :: CJITModule -> Ptr CJassType -> IO CInt

cReal :: CJITModule -> Ptr CJassType -> IO CInt 
cReal = getTypeId JReal
foreign export ccall "hjassReal" cReal :: CJITModule -> Ptr CJassType -> IO CInt 

cBoolean :: CJITModule -> Ptr CJassType -> IO CInt 
cBoolean = getTypeId JBoolean
foreign export ccall "hjassBoolean" cBoolean :: CJITModule -> Ptr CJassType -> IO CInt 

cString :: CJITModule -> Ptr CJassType -> IO CInt 
cString = getTypeId JString
foreign export ccall "hjassString" cString :: CJITModule -> Ptr CJassType -> IO CInt 

cHandle :: CJITModule -> Ptr CJassType -> IO CInt 
cHandle = getTypeId JHandle
foreign export ccall "hjassHandle" cHandle :: CJITModule -> Ptr CJassType -> IO CInt 

cCode :: CJITModule -> Ptr CJassType -> IO CInt 
cCode = getTypeId JCode
foreign export ccall "hjassCode" cCode :: CJITModule -> Ptr CJassType -> IO CInt 

cUserDefinedType :: CJITModule -> CString -> Ptr CJassType -> IO CInt 
cUserDefinedType cjit cname retptr = do
  jt <- JUserDefined <$> peekCString cname
  getTypeId jt cjit retptr 
foreign export ccall "hjassUserDefinedType" cUserDefinedType :: CJITModule -> CString -> Ptr CJassType -> IO CInt

cPrintType :: CJITModule -> CJassType -> Ptr CString -> IO CInt
cPrintType cjit cjt retptr = saveZeroBasedError $ do
  jt <- getTypeFromId cjit cjt
  liftIO $ poke retptr =<< newCString (show jt)
  return 1
foreign export ccall "hjassPrintType" cPrintType :: CJITModule -> CJassType -> Ptr CString -> IO CInt