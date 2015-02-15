module Language.Jass.CAPI.JIT.Calling(

  ) where

import Language.Jass.CAPI.Base
import Language.Jass.CAPI.JIT.Module
import Language.Jass.JIT.Calling
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.IO.Class

cCallFunc0 :: Storable a => CJITModule -> CString -> Ptr a -> IO CInt 
cCallFunc1 :: Storable b => CJITModule -> CString -> a -> Ptr b -> IO CInt 
cCallFunc2 :: Storable c => CJITModule -> CString -> a -> b -> Ptr c -> IO CInt
cCallFunc3 :: Storable d => CJITModule -> CString -> a -> b -> c -> Ptr d -> IO CInt
cCallFunc4 :: Storable e => CJITModule -> CString -> a -> b -> c -> d -> Ptr e -> IO CInt
cCallFunc5 :: Storable f => CJITModule -> CString -> a -> b -> c -> d -> e -> Ptr f -> IO CInt
cCallFunc6 :: Storable g => CJITModule -> CString -> a -> b -> c -> d -> e -> f -> Ptr g -> IO CInt
cCallFunc7 :: Storable i => CJITModule -> CString -> a -> b -> c -> d -> e -> f -> g -> Ptr i -> IO CInt

foreign import ccall "dynamic" mkFunc0 :: FunPtr (IO a) -> IO a
  
cCallFunc0 cjit cname resptr = cCallFunc' cjit cname $ \jit nativeName -> poke' resptr =<< callFunc0 jit nativeName mkFunc0
cCallFunc1 cjit cname arg1 resptr = cCallFunc' cjit cname $ \jit nativeName -> poke' resptr =<< callFunc1 jit nativeName fptr arg1
cCallFunc2 cjit cname arg1 arg2 resptr = cCallFunc' cjit cname $ \jit nativeName -> poke' resptr =<< callFunc2 jit nativeName fptr arg1 arg2
cCallFunc3 cjit cname arg1 arg2 arg3 resptr = cCallFunc' cjit cname $ \jit nativeName -> poke' resptr =<< callFunc3 jit nativeName fptr arg1 arg2 arg3
cCallFunc4 cjit cname arg1 arg2 arg3 arg4 resptr = cCallFunc' cjit cname $ \jit nativeName -> poke' resptr =<< callFunc4 jit nativeName fptr arg1 arg2 arg3 arg4
cCallFunc5 cjit cname arg1 arg2 arg3 arg4 arg5 resptr = cCallFunc' cjit cname $ \jit nativeName -> poke' resptr =<< callFunc5 jit nativeName fptr arg1 arg2 arg3 arg4 arg5
cCallFunc6 cjit cname arg1 arg2 arg3 arg4 arg5 arg6 resptr = cCallFunc' cjit cname $ \jit nativeName -> poke' resptr =<< callFunc6 jit nativeName fptr arg1 arg2 arg3 arg4 arg5 arg6
cCallFunc7 cjit cname arg1 arg2 arg3 arg4 arg5 arg6 arg7 resptr = cCallFunc' cjit cname $ \jit nativeName -> poke' resptr =<< callFunc7 jit nativeName fptr arg1 arg2 arg3 arg4 arg5 arg6 arg7

poke' ptr v = liftIO $ poke ptr v

cCallFunc' cjit cname action = do
  mjit <- lookupJITModuleTable cjit
  nativeName <- peekCString cname
  case mjit of
    Nothing -> setLastError "invalid jit module handle" >> return 0
    Just jit -> saveZeroBasedError $ action jit nativeName >> return 1

foreign export ccall "hjassCallFunc0Int"    cCallFunc0 :: CJITModule -> CString -> FunPtr (IO CInt)    -> Ptr CInt    -> IO CInt
--foreign export ccall "hjassCallFunc0Real"   cCallFunc0 :: CJITModule -> CString -> (FunPtr (IO CFloat)  -> IO CFloat)  -> Ptr CFloat  -> IO CInt 
--foreign export ccall "hjassCallFunc0Bool"   cCallFunc0 :: CJITModule -> CString -> (FunPtr (IO CChar)   -> IO CChar)   -> Ptr CChar   -> IO CInt
--foreign export ccall "hjassCallFunc0Str"    cCallFunc0 :: CJITModule -> CString -> (FunPtr (IO CString) -> IO CString) -> Ptr CString -> IO CInt
--foreign export ccall "hjassCallFunc0Handle" cCallFunc0 :: CJITModule -> CString -> (FunPtr (IO CLong)   -> IO CLong)   -> Ptr CLong   -> IO CInt
--foreign export ccall "hjassCallFunc0Code"   cCallFunc0 :: CJITModule -> CString -> (FunPtr (IO CLong)   -> IO CLong)   -> Ptr CLong   -> IO CInt
--
--foreign export ccall "hjassCallFunc1Int"    cCallFunc1 :: CJITModule -> CString -> (FunPtr (a -> IO CInt)    -> a -> IO CInt)    -> a -> Ptr CInt    -> IO CInt
--foreign export ccall "hjassCallFunc1Real"   cCallFunc1 :: CJITModule -> CString -> (FunPtr (a -> IO CFloat)  -> a -> IO CFloat)  -> a -> Ptr CFloat  -> IO CInt 
--foreign export ccall "hjassCallFunc1Bool"   cCallFunc1 :: CJITModule -> CString -> (FunPtr (a -> IO CChar)   -> a -> IO CChar)   -> a -> Ptr CChar   -> IO CInt
--foreign export ccall "hjassCallFunc1Str"    cCallFunc1 :: CJITModule -> CString -> (FunPtr (a -> IO CString) -> a -> IO CString) -> a -> Ptr CString -> IO CInt
--foreign export ccall "hjassCallFunc1Handle" cCallFunc1 :: CJITModule -> CString -> (FunPtr (a -> IO CLong)   -> a -> IO CLong)   -> a -> Ptr CLong   -> IO CInt
--foreign export ccall "hjassCallFunc1Code"   cCallFunc1 :: CJITModule -> CString -> (FunPtr (a -> IO CLong)   -> a -> IO CLong)   -> a -> Ptr CLong   -> IO CInt
--
--foreign export ccall "hjassCallFunc2Int"    cCallFunc2 :: CJITModule -> CString -> (FunPtr (a -> b -> IO CInt)    -> a -> b -> IO CInt)    -> a -> b -> Ptr CInt    -> IO CInt
--foreign export ccall "hjassCallFunc2Real"   cCallFunc2 :: CJITModule -> CString -> (FunPtr (a -> b -> IO CFloat)  -> a -> b -> IO CFloat)  -> a -> b -> Ptr CFloat  -> IO CInt 
--foreign export ccall "hjassCallFunc2Bool"   cCallFunc2 :: CJITModule -> CString -> (FunPtr (a -> b -> IO CChar)   -> a -> b -> IO CChar)   -> a -> b -> Ptr CChar   -> IO CInt
--foreign export ccall "hjassCallFunc2Str"    cCallFunc2 :: CJITModule -> CString -> (FunPtr (a -> b -> IO CString) -> a -> b -> IO CString) -> a -> b -> Ptr CString -> IO CInt
--foreign export ccall "hjassCallFunc2Handle" cCallFunc2 :: CJITModule -> CString -> (FunPtr (a -> b -> IO CLong)   -> a -> b -> IO CLong)   -> a -> b -> Ptr CLong   -> IO CInt
--foreign export ccall "hjassCallFunc2Code"   cCallFunc2 :: CJITModule -> CString -> (FunPtr (a -> b -> IO CLong)   -> a -> b -> IO CLong)   -> a -> b -> Ptr CLong   -> IO CInt
--
--foreign export ccall "hjassCallFunc3Int"    cCallFunc3 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> IO CInt)    -> a -> b -> c -> IO CInt)    -> a -> b -> c -> Ptr CInt    -> IO CInt
--foreign export ccall "hjassCallFunc3Real"   cCallFunc3 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> IO CFloat)  -> a -> b -> c -> IO CFloat)  -> a -> b -> c -> Ptr CFloat  -> IO CInt 
--foreign export ccall "hjassCallFunc3Bool"   cCallFunc3 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> IO CChar)   -> a -> b -> c -> IO CChar)   -> a -> b -> c -> Ptr CChar   -> IO CInt
--foreign export ccall "hjassCallFunc3Str"    cCallFunc3 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> IO CString) -> a -> b -> c -> IO CString) -> a -> b -> c -> Ptr CString -> IO CInt
--foreign export ccall "hjassCallFunc3Handle" cCallFunc3 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> IO CLong)   -> a -> b -> c -> IO CLong)   -> a -> b -> c -> Ptr CLong   -> IO CInt
--foreign export ccall "hjassCallFunc3Code"   cCallFunc3 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> IO CLong)   -> a -> b -> c -> IO CLong)   -> a -> b -> c -> Ptr CLong   -> IO CInt
--
--foreign export ccall "hjassCallFunc4Int"    cCallFunc4 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> d -> IO CInt)    -> a -> b -> c -> d -> IO CInt)    -> a -> b -> c -> d -> Ptr CInt    -> IO CInt
--foreign export ccall "hjassCallFunc4Real"   cCallFunc4 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> d -> IO CFloat)  -> a -> b -> c -> d -> IO CFloat)  -> a -> b -> c -> d -> Ptr CFloat  -> IO CInt 
--foreign export ccall "hjassCallFunc4Bool"   cCallFunc4 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> d -> IO CChar)   -> a -> b -> c -> d -> IO CChar)   -> a -> b -> c -> d -> Ptr CChar   -> IO CInt
--foreign export ccall "hjassCallFunc4Str"    cCallFunc4 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> d -> IO CString) -> a -> b -> c -> d -> IO CString) -> a -> b -> c -> d -> Ptr CString -> IO CInt
--foreign export ccall "hjassCallFunc4Handle" cCallFunc4 :: CJITModule -> CString -> (FunPtr (a -> b -> c -> d -> IO CLong)   -> a -> b -> c -> d -> IO CLong)   -> a -> b -> c -> d -> Ptr CLong   -> IO CInt