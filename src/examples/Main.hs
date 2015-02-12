module Main where

import Language.Jass.Program

import Control.Monad.Trans.Except
import Control.Applicative
import Natives 

runProgramm :: String -> ExceptT String IO ()
runProgramm mainPath = do
  prog <- constructProgramFromSource "tests/rosetta" mainPath
  executeProgram prog makeNativeTable callMain
    
programs :: [(String, String)]
programs = [
  ("100 doors", "hundredDoors.j"),
  ("24 game", "twentyFourGame.j")
  ]
  
main :: IO ()
main = do
  mapM_ putStrLn ((\(i,n) -> show i ++ ". " ++ n) <$> [(1 :: Int)..] `zip` fmap fst programs)
  putStrLn "Print number of program: " 
  i <- read <$> getLine
  exceptIO $ runProgramm $ snd $ programs !! (i-1)