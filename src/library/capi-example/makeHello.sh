export GHC_VER=7.8.3
export GHC_PATH=/usr/local/lib

ln -s ../../../.cabal-sandbox/lib/*/*/*.so .
ln -s $GHC_PATH/ghc-$GHC_VER/rts-1.0/libHSrts-ghc$GHC_VER.so .
ln -s ../../../.dist-buildwrapper/dist/build/libHShjass-0.1.0.0-ghc$GHC_VER.so .
gcc -o hello hello.c -L. -lHShjass-0.1.0.0-ghc$GHC_VER -lHSrts-ghc$GHC_VER -Wl,-rpath -Wl,.
