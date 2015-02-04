-- ghc --make -dynamic -shared -fPIC hello_natives.hs -o libhello_natives.so
module Natives where

import Foreign.C.String

init :: IO ()
init = return ()

nativeWriteln :: CString -> IO ()
nativeWriteln adr = putStrLn =<< peekCString adr

foreign export ccall "writeln" nativeWriteln :: CString -> IO ()