module Language.Jass.Parser.GrammarTest where
import Language.Jass.Parser.Grammar
import Language.Jass.Parser.AST

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

parseTest :: String -> JassModule -> Assertion
parseTest input rightTree= case parseJass "test" input of
    Left err -> error (show err)
    Right tree -> tree @?= rightTree
     
syntaxTests :: TestTree
syntaxTests = testGroup "Syntax tests"
    [ testCase "Type declarations" $
        parseTest
            ("type widget extends handle\n" ++
            "type destructable extends widget")
            (JassModule [
                TypeDef "widget" JHandle,
                TypeDef "destructable" (JUserDefined "widget")
            ] [] [] [])
    ]