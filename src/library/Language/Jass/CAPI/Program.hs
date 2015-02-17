module Language.Jass.CAPI.Program(

  ) where

import Language.Jass.CAPI.Base
import Language.Jass.CAPI.JIT.Module
import Language.Jass.Utils
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Language.Jass.Program
import qualified Data.HashMap.Strict as HM
import Control.Exception
import Control.Monad.Trans.Except

cConstructProgram ::
  Ptr CString -- ^ Array of strings with module names
  -> Ptr CString -- ^ Array of strings with module sources
  -> CInt -- ^ Size of arrays
  -> CString -- ^ Name of main module
  -> IO CProgram -- ^ zero return is error, error could be read via hjassGetLastError
cConstructProgram cmodNames cmodSources csize cmainName = do
  let size = fromIntegral csize
  modNames <- mapM peekCString =<< peekArray size cmodNames
  modSources <- mapM peekCString =<< peekArray size cmodSources
  mainName <- peekCString cmainName
  saveZeroBasedError $ do 
    prog <- constructProgram (HM.fromList $ modNames `zip` modSources) mainName
    insertIntoProgramTable prog

foreign export ccall "hjassConstructProgram" cConstructProgram :: Ptr CString -> Ptr CString -> CInt -> CString -> IO CProgram

cConstructProgramFromSource ::
  CString -- ^ Path to folder
  -> CString -- ^ Relative path to folder
  -> IO CProgram -- ^ zero return is error, error could be read via hjassGetLastError
cConstructProgramFromSource cfolder cmain = do
  folder <- peekCString cfolder
  main <- peekCString cmain
  saveZeroBasedError $ do
    prog <- constructProgramFromSource folder main
    insertIntoProgramTable prog
    
foreign export ccall "hjassConstructProgramFromSource" cConstructProgramFromSource :: CString -> CString -> IO CProgram

type CNativeMaker = 
  CJITModule -- ^ Descriptor of executing module 
  -> Ptr (Ptr CString) -- ^ [out parameter] pointer to array of string's - names of natives   
  -> Ptr (Ptr (FunPtr ())) -- ^ [out parameter] pointer to array of natives pointers
  -> Ptr  CString -- ^ [out parameter] if function returns negative size, the string is used as a error message 
  -> IO CInt -- ^ size of returned arrays, negative is threaded as error and message is taken from last parameter
foreign import ccall "dynamic"
  mkCNativeMaker :: FunPtr CNativeMaker -> CNativeMaker

makeNativeMaker :: FunPtr CNativeMaker -> NativeTableMaker
makeNativeMaker fptr jit = liftExcept $ bracket (insertIntoJITModuleTable jit) freeJITModuleTable $ \cjit -> 
  alloca $ \cnativesNames -> alloca $ \cnativesPtrs -> alloca $ \cerrorMsg -> do
    csize <- mkCNativeMaker fptr cjit cnativesNames cnativesPtrs cerrorMsg
    if csize < 0 
    then do
      errMsg <- peekCString =<< peek cerrorMsg
      return $ Left errMsg
    else do
      nativesNames <- mapM peekCString =<< peekArray (fromIntegral csize) =<< peek cnativesNames
      nativesPtrs <- peekArray (fromIntegral csize) =<< peek cnativesPtrs
      return $ Right $ nativesNames `zip` nativesPtrs
      
type CExecutingCallback =
  CJITModule -- ^ Descriptor of executing module
  -> Ptr CString -- ^ If result is zero, the error message in the argument is used
  -> IO CInt -- ^ status code, zero is an error, message is taken from last parameter
foreign import ccall "dynamic"
  mkCExecutingCallback :: FunPtr CExecutingCallback -> CExecutingCallback

makeExecutingCallback :: FunPtr CExecutingCallback -> JITModule -> ExceptT String IO CInt
makeExecutingCallback fptr jit = liftExcept $ bracket (insertIntoJITModuleTable jit) freeJITModuleTable $ \cjit -> 
  alloca $ \cerrorMsg -> do
    cstatus <- mkCExecutingCallback fptr cjit cerrorMsg
    if cstatus == 0 
    then do
      errMsg <- peekCString =<< peek cerrorMsg
      return $ Left errMsg
    else return $ Right 1
      
cExecuteProgram :: CProgram -- ^ Descriptor of program 
  -> FunPtr CNativeMaker -- ^ native table callback
  -> FunPtr CExecutingCallback -- ^ executing callback
  -> IO CInt -- ^ zero return is error, error could be read via hjassGetLastError
cExecuteProgram cprog cnativeMaker cexecCallback = do
  mprog <- lookupProgramTable cprog
  case mprog of
    Nothing -> setLastError "invalid program handle" >> return 0
    Just prog -> saveZeroBasedError $ 
      executeProgram prog (makeNativeMaker cnativeMaker) (makeExecutingCallback cexecCallback) 
      
foreign export ccall "hjassExecuteProgram" cExecuteProgram :: CProgram -> FunPtr CNativeMaker -> FunPtr CExecutingCallback -> IO CInt

-- | Removing program by handle, should be called by user    
cFreeProgram :: CProgram -> IO ()
cFreeProgram = freeProgramTable

foreign export ccall "hjassFreeProgram" cFreeProgram :: CProgram -> IO ()
