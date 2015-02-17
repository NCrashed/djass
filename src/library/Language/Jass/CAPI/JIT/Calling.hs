module Language.Jass.CAPI.JIT.Calling(

  ) where

import Language.Jass.CAPI.Base
import Language.Jass.CAPI.JIT.Module
import Language.Jass.JIT.Module
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import LLVM.General.ExecutionEngine
import LLVM.General.AST

cGetJassFuncPtr :: CJITModule -> CString -> Ptr (FunPtr ()) -> IO CInt
cGetJassFuncPtr cjit cname fptrPtr = do
  mjit <- lookupJITModuleTable cjit
  funcName <- peekCString cname
  case mjit of
    Nothing -> setLastError "invalid jit module handle" >> return 0
    Just (JITModule _ exModule) -> do
      mfptr <- getFunction exModule (Name funcName)
      case mfptr of
        Nothing -> do
          setLastError ("cannot find function with name " ++ funcName) 
          return 0
        Just fptr -> do
          poke fptrPtr fptr
          return 1
        
foreign export ccall "hjassGetJassFuncPtr" cGetJassFuncPtr :: CJITModule -> CString -> Ptr (FunPtr ()) -> IO CInt