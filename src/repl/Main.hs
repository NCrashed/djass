module Main where

import Language.Jass.Parser.Grammar
import Paths_hjass

main :: IO ()
main = do
    res <- parseJassFile =<< getDataFileName "tests/common.j"
    case res of
        Left err -> print err
        Right tree -> print tree
