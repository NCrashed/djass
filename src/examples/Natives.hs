{-# LANGUAGE TupleSections #-}
module Natives where

import Language.Jass.Program
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.Random  
import Data.Char

exceptIO :: ExceptT String IO a -> IO ()
exceptIO action = do
  res <- runExceptT action
  case res of
    Left err -> putStrLn err
    Right _ -> return ()
    
type NativeWriteln = CString -> IO ()
nativeWriteln :: NativeWriteln
nativeWriteln adr = putStrLn =<< fromJass adr
foreign import ccall "wrapper" mkNativeWriteln :: NativeWriteln -> IO (FunPtr NativeWriteln)

type NativeWrite = CString -> IO ()
nativeWrite :: NativeWrite
nativeWrite adr = putStr =<< fromJass adr
foreign import ccall "wrapper" mkNativeWrite :: NativeWrite -> IO (FunPtr NativeWrite)

type NativeReadln = IO CString
nativeReadln :: NativeReadln
nativeReadln = toJass =<< getLine
foreign import ccall "wrapper" mkNativeReadln :: NativeReadln -> IO (FunPtr NativeReadln)

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

type NativeGetStringLength = CString -> IO CInt
nativeGetStringLength :: NativeGetStringLength
nativeGetStringLength cs = toJass.length =<< fromJass cs
foreign import ccall "wrapper" mkNativeGetStringLength :: NativeGetStringLength -> IO (FunPtr NativeGetStringLength)

type NativeGetSubstring = CString -> CInt -> CInt -> IO CString
nativeGetSubstring :: NativeGetSubstring
nativeGetSubstring cs cibeg cicount
  | cibeg < 0 || cicount <= 0 = toJass "" 
  | otherwise = do
    s <- fromJass cs
    ibeg <- fromJass cibeg
    icount <- fromJass cicount
    if length s < ibeg && length s < ibeg + icount then toJass ""
    else toJass $ take icount $ drop ibeg s 
foreign import ccall "wrapper" mkNativeGetSubstring :: NativeGetSubstring -> IO (FunPtr NativeGetSubstring)

type NativeGetChar = CString -> CInt -> IO CInt
nativeGetChar :: NativeGetChar
nativeGetChar cs ci 
  | ci < 0 = toJass (0 :: Int)
  | otherwise = do
    s <- fromJass cs
    i <- fromJass ci
    if length s < i then toJass (0 :: Int)
    else toJass $ ord $ s !! i
foreign import ccall "wrapper" mkNativeGetChar :: NativeGetChar -> IO (FunPtr NativeGetChar)

type NativeC2S = CInt -> IO CString
nativeC2S :: NativeC2S
nativeC2S i = toJass.(\c->[c]).chr =<< fromJass i
foreign import ccall "wrapper" mkNativeC2S :: NativeC2S -> IO (FunPtr NativeC2S)

type NativeS2C = CString -> IO CInt
nativeS2C :: NativeS2C
nativeS2C cs = do
  s <- fromJass cs
  if length s == 0 then toJass (0 :: Int)
  else toJass $ ord (head s)
foreign import ccall "wrapper" mkNativeS2C :: NativeS2C -> IO (FunPtr NativeS2C)

type ExecutingFunc = IO ()
foreign import ccall "dynamic"
  mkExecutingFunc :: FunPtr ExecutingFunc -> ExecutingFunc
  
type NativeExecute = CString -> IO ()
nativeExecute :: JITModule -> NativeExecute
nativeExecute jit cname = do
  fname <- fromJass cname
  exceptIO $ callFunc0 jit fname mkExecutingFunc
  
foreign import ccall "wrapper" mkNativeExecute :: NativeExecute -> IO (FunPtr NativeExecute)

makeNativeTable :: NativeTableMaker
makeNativeTable jit = liftIO $ sequence [ ("writeln",) . castFunPtr <$> mkNativeWriteln nativeWriteln
                                      , ("write",) . castFunPtr <$> mkNativeWrite nativeWrite
                                      , ("readln",) . castFunPtr <$> mkNativeReadln nativeReadln
                                      , ("I2S",) . castFunPtr <$> mkNativeI2S nativeI2S
                                      , ("R2S",) . castFunPtr <$> mkNativeR2S nativeR2S
                                      , ("S2I",) . castFunPtr <$> mkNativeS2I nativeS2I
                                      , ("S2R",) . castFunPtr <$> mkNativeS2R nativeS2R
                                      , ("getRandInt",) . castFunPtr <$> mkNativeGetRandInt nativeGetRandInt
                                      , ("getRandReal",) . castFunPtr <$> mkNativeGetRandReal nativeGetRandReal
                                      , ("getRandBool",) . castFunPtr <$> mkNativeGetRandBool nativeGetRandBool
                                      , ("getStringLength",) . castFunPtr <$> mkNativeGetStringLength nativeGetStringLength
                                      , ("getSubString",) . castFunPtr <$> mkNativeGetSubstring nativeGetSubstring
                                      , ("getChar",) . castFunPtr <$> mkNativeGetChar nativeGetChar
                                      , ("C2S",) . castFunPtr <$> mkNativeC2S nativeC2S
                                      , ("S2C",) . castFunPtr <$> mkNativeS2C nativeS2C 
                                      , ("execute",) . castFunPtr <$> mkNativeExecute (nativeExecute jit)]
