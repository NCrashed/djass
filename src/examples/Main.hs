module Main where

import Language.Jass.Program
import Control.Monad.Trans.Except
import Natives 

runProgramm :: String -> ExceptT String IO ()
runProgramm mainPath = do
  prog <- constructProgramFromSource "tests/rosetta" mainPath
  executeProgram prog makeNativeTable callMain

exceptIO :: ExceptT String IO a -> IO ()
exceptIO action = do
  res <- runExceptT action
  case res of
    Left err -> putStrLn err
    Right _ -> return ()
      
main :: IO ()
main = exceptIO $ runProgramm "hundredDoors.j"
