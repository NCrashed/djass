ln -s ../../../.cabal-sandbox/lib/*/*/*.so .
ln -s /usr/local/lib/ghc-7.8.4/rts-1.0/libHSrts-ghc7.8.4.so .
ln -s ../../../.dist-buildwrapper/dist/build/*.so .
gcc -o hello hello.c -L. -lHShjass-0.1.0.0-ghc7.8.4 -lHSrts-ghc7.8.4 -Wl,-rpath,$ORIGIN
