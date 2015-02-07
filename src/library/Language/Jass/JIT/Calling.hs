{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
-- | Unfinished module that should simplify interfacing with jass code
module Language.Jass.JIT.Calling(
    ToJassConv(..)
  , FromJassConv(..)
  , callMain
  , callFunc0
  , callFunc1
  , callFunc2
  , callFunc3
  , callFunc4
  , callFunc5
  , callFunc6
  , callFunc7
  ) where

import Language.Jass.JIT.Module
import GHC.Float
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import LLVM.General.ExecutionEngine
import LLVM.General.AST
  
callFunc0 :: JITModule -> String -> (FunPtr (IO a) -> IO a) -> ExceptT String IO a
callFunc1 :: JITModule -> String -> (FunPtr (a -> IO b) -> a -> IO b) -> a -> ExceptT String IO b
callFunc2 :: JITModule -> String -> (FunPtr (a -> b -> IO c) -> a -> b -> IO c) -> a -> b -> ExceptT String IO c
callFunc3 :: JITModule -> String -> (FunPtr (a -> b -> c -> IO d) -> a -> b -> c -> IO d) -> a -> b -> c -> ExceptT String IO d
callFunc4 :: JITModule -> String -> (FunPtr (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> ExceptT String IO e
callFunc5 :: JITModule -> String -> (FunPtr (a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> IO f) -> a -> b -> c -> d -> e -> ExceptT String IO f
callFunc6 :: JITModule -> String -> (FunPtr (a -> b -> c -> d -> e -> f -> IO g) -> a -> b -> c -> d -> e -> f -> IO g) -> a -> b -> c -> d -> e -> f -> ExceptT String IO g
callFunc7 :: JITModule -> String -> (FunPtr (a -> b -> c -> d -> e -> f -> g -> IO i) -> a -> b -> c -> d -> e -> f -> g -> IO i) -> a -> b -> c -> d -> e -> f -> g -> ExceptT String IO i

callFunc0 exModule funcName funcMaker = callFunc exModule funcName $ \ptr -> liftIO $ funcMaker $ castFunPtr ptr
callFunc1 exModule funcName funcMaker arg1 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1
callFunc2 exModule funcName funcMaker arg1 arg2 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2
callFunc3 exModule funcName funcMaker arg1 arg2 arg3 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3
callFunc4 exModule funcName funcMaker arg1 arg2 arg3 arg4 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3 arg4
callFunc5 exModule funcName funcMaker arg1 arg2 arg3 arg4 arg5 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3 arg4 arg5
callFunc6 exModule funcName funcMaker arg1 arg2 arg3 arg4 arg5 arg6 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3 arg4 arg5 arg6
callFunc7 exModule funcName funcMaker arg1 arg2 arg3 arg4 arg5 arg6 arg7 = callFunc exModule funcName $ \ptr -> liftIO $ (funcMaker $ castFunPtr ptr) arg1 arg2 arg3 arg4 arg5 arg6 arg7

callFunc :: JITModule -> String -> (FunPtr () -> ExceptT String IO a) -> ExceptT String IO a 
callFunc (JITModule exModule) funcName action = do
  mptr <- liftIO $ getFunction exModule (Name funcName)
  case mptr of
    Nothing -> throwE $ "Cannot find "++ funcName ++" function in jass module!"
    Just ptr -> action ptr

type JassMain = IO ()
foreign import ccall "dynamic"
  mkJassMain :: FunPtr JassMain -> JassMain  
  
callMain :: JITModule -> ExceptT String IO ()
callMain module' = callFunc0 module' "main" mkJassMain

-- | Describes convertion to Jass runtime represenation of types
class ToJassConv a where
  type JassSide a :: *
  toJass          :: MonadIO m => a -> m (JassSide a) 

instance ToJassConv () where
  type JassSide () = ()
  toJass _ = return ()
  
instance ToJassConv Int where
  type JassSide Int = CInt
  toJass = return . CInt . fromIntegral

instance ToJassConv Integer where
  type JassSide Integer = CInt
  toJass = return . CInt . fromIntegral
    
instance ToJassConv Float where
  type JassSide Float = CFloat
  toJass = return . CFloat 
  
instance ToJassConv Double where
  type JassSide Double = CFloat
  toJass = return . CFloat . double2Float
  
instance ToJassConv String where
  type JassSide String = CString
  toJass s = liftIO $ newCString s -- | TODO: need to free

instance ToJassConv Bool where
  type JassSide Bool = CChar
  toJass = return . CChar . toInt
    where toInt b = if b then 1 else 0
    
-- | TODO: handle and code types

-- | Describes convertion from Jass runtime representation of types
class FromJassConv a where
  type HaskellSide a :: *
  fromJass           :: MonadIO m => a -> m (HaskellSide a)

instance FromJassConv () where
  type HaskellSide () = ()
  fromJass _ = return ()
  
instance FromJassConv CInt where
  type HaskellSide CInt = Int
  fromJass = return . fromIntegral
  
instance FromJassConv CFloat where
  type HaskellSide CFloat = Float
  fromJass (CFloat f) = return f
  
instance FromJassConv CString where
  type HaskellSide CString = String
  fromJass s = liftIO $ peekCString s
  
instance FromJassConv CChar where
  type HaskellSide CChar = Bool
  fromJass (CChar i) = return $ i /= 0
  
-- | TODO: handle and code types