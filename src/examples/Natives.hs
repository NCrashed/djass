{-# LANGUAGE TupleSections #-}
module Natives where

import Language.Jass.Program
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Control.Applicative
import Control.Monad.IO.Class
import System.Random  

type NativeWriteln = CString -> IO ()
nativeWriteln :: NativeWriteln
nativeWriteln adr = putStrLn =<< fromJass adr
foreign import ccall "wrapper" mkNativeWriteln :: NativeWriteln -> IO (FunPtr NativeWriteln)

type NativeWrite = CString -> IO ()
nativeWrite :: NativeWrite
nativeWrite adr = putStr =<< fromJass adr
foreign import ccall "wrapper" mkNativeWrite :: NativeWrite -> IO (FunPtr NativeWrite)


type NativeI2S = CInt -> IO CString
nativeI2S :: NativeI2S
nativeI2S i = toJass.show =<< fromJass i
foreign import ccall "wrapper" mkNativeI2S :: NativeI2S -> IO (FunPtr NativeI2S)

type NativeR2S = CFloat -> IO CString
nativeR2S :: NativeR2S
nativeR2S i = toJass.show =<< fromJass i
foreign import ccall "wrapper" mkNativeR2S :: NativeR2S -> IO (FunPtr NativeR2S)

type NativeS2I = CString -> IO CInt
nativeS2I :: NativeS2I
nativeS2I s = toJass.(read :: String -> Int) =<< fromJass s
foreign import ccall "wrapper" mkNativeS2I :: NativeS2I -> IO (FunPtr NativeS2I)

type NativeS2R = CString -> IO CFloat
nativeS2R :: NativeS2R
nativeS2R s = toJass.(read :: String -> Float) =<< fromJass s
foreign import ccall "wrapper" mkNativeS2R :: NativeS2R -> IO (FunPtr NativeS2R)

type NativeGetRandInt = CInt -> CInt -> IO CInt
nativeGetRandInt :: NativeGetRandInt
nativeGetRandInt low high 
  | low > high = return 0
  | otherwise  = curry randomRIO low high  
foreign import ccall "wrapper" mkNativeGetRandInt :: NativeGetRandInt -> IO (FunPtr NativeGetRandInt)

type NativeGetRandReal = CFloat -> CFloat -> IO CFloat
nativeGetRandReal :: NativeGetRandReal
nativeGetRandReal low high 
  | low > high = return 0
  | otherwise  = curry randomRIO low high  
foreign import ccall "wrapper" mkNativeGetRandReal :: NativeGetRandReal -> IO (FunPtr NativeGetRandReal)

type NativeGetRandBool = IO CChar
nativeGetRandBool :: NativeGetRandBool
nativeGetRandBool = randomRIO (0, 1)
foreign import ccall "wrapper" mkNativeGetRandBool :: NativeGetRandBool -> IO (FunPtr NativeGetRandBool)

makeNativeTable :: NativeTableMaker
makeNativeTable _ = liftIO $ sequence [ ("writeln",) . castFunPtr <$> mkNativeWriteln nativeWriteln
                                      , ("write",) . castFunPtr <$> mkNativeWrite nativeWrite
                                      , ("I2S",) . castFunPtr <$> mkNativeI2S nativeI2S
                                      , ("R2S",) . castFunPtr <$> mkNativeR2S nativeR2S
                                      , ("S2I",) . castFunPtr <$> mkNativeS2I nativeS2I
                                      , ("S2R",) . castFunPtr <$> mkNativeS2R nativeS2R
                                      , ("getRandInt",) . castFunPtr <$> mkNativeGetRandInt nativeGetRandInt
                                      , ("getRandReal",) . castFunPtr <$> mkNativeGetRandReal nativeGetRandReal
                                      , ("getRandBool",) . castFunPtr <$> mkNativeGetRandBool nativeGetRandBool ]
