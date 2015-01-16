module Language.Jass.Parser.GrammarTest where
import Language.Jass.Parser.Grammar
import Language.Jass.Parser.AST

import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

parseTest :: (Show b, Show a, Eq a) => (String -> String -> Either b a) -> String -> a -> Assertion
parseTest f input rightTree= case f "test" input of
    Left err -> error (show err)
    Right tree -> tree @?= rightTree
    
parseTestModule :: String -> JassModule -> Assertion
parseTestModule = parseTest parseJass 

parseTestFunction :: String -> Function -> Assertion
parseTestFunction = parseTest parseJassFunction 
   
parseTestLocalVar :: String -> LocalVar -> Assertion
parseTestLocalVar = parseTest parseJassLocalVar 

parseTestGlobalVar :: String -> GlobalVar -> Assertion
parseTestGlobalVar = parseTest parseJassGlobalVar 

parseTestTwoGlobalVars :: String -> (GlobalVar, GlobalVar) -> Assertion
parseTestTwoGlobalVars = parseTest parseJassTwoGlobalVars 

parseTestExpression :: String -> Expression -> Assertion
parseTestExpression = parseTest parseJassExpression 

parseTestStatement :: String -> Statement -> Assertion
parseTestStatement = parseTest parseJassStatement

testSimple :: (String -> a -> Assertion) -> String -> a -> TestTree
testSimple f input answer = testCase input $ f input answer
  
testExpression :: String -> Expression -> TestTree
testExpression = testSimple parseTestExpression

testStatement :: String -> Statement -> TestTree
testStatement = testSimple parseTestStatement

testJassFile :: FilePath -> Assertion
testJassFile path = do 
  res <- parseJassFile path
  case res of
    Left err -> assertFailure (show err)
    Right _ -> return ()
    
commonParsing :: TestTree
commonParsing = testGroup "common.j and blizzard.j parsing"
  [ testCase "common.j" $ testJassFile "tests/common.j",
    testCase "blizzard.j" $ testJassFile "tests/blizzard.j"
  ]

syntaxTests :: TestTree
syntaxTests = testGroup "Syntax tests"
    [ testCase "empty module" $ parseTestModule "" (JassModule [] [] [] []),
      testCase "Type declarations" $
        parseTestModule
            ("type widget extends handle\n" ++
            "type destructable extends widget")
            (JassModule [
                TypeDef "widget" JHandle,
                TypeDef "destructable" (JUserDefined "widget")
            ] [] [] []),
      testCase "global variable" $
        parseTestGlobalVar "force bj_FORCE_ALL_PLAYERS = null" 
          (GlobalVar False False (JUserDefined "force") "bj_FORCE_ALL_PLAYERS" (Just NullLiteral)),
      testCase "globals" $
        parseTestTwoGlobalVars
          ("constant integer   bj_ELEVATOR_WALL_TYPE_WEST       = 4\n" ++
           "force              bj_FORCE_ALL_PLAYERS        = null\n")
          (
            GlobalVar True False JInteger "bj_ELEVATOR_WALL_TYPE_WEST" (Just $ IntegerLiteral 4),
            GlobalVar False False (JUserDefined "force") "bj_FORCE_ALL_PLAYERS" (Just NullLiteral)
          ),
      testCase "globals" $
        parseTestModule
          ("globals\n" ++
           "real global1\n" ++
           "integer global2 = 0\n" ++
           "constant real global3 = 3.14\n" ++
           "handle array global4\n" ++
           "constant widget array global5\n" ++ 
           "constant playercolor global6 = ConvertPlayerColor(0)\n" ++ 
           "constant integer   bj_ELEVATOR_WALL_TYPE_WEST       = 4\n" ++
           "force              bj_FORCE_ALL_PLAYERS        = null\n" ++
           "endglobals")
          (JassModule [] [
            GlobalVar False False JReal "global1" Nothing,
            GlobalVar False False JInteger "global2" (Just $ IntegerLiteral 0),
            GlobalVar True False JReal "global3" (Just $ RealLiteral 3.14),
            GlobalVar False True JHandle "global4" Nothing,
            GlobalVar True True (JUserDefined "widget") "global5" Nothing,
            GlobalVar True False (JUserDefined "playercolor") "global6" (Just $ FunctionCall "ConvertPlayerColor" [IntegerLiteral 0]),
            GlobalVar True False JInteger "bj_ELEVATOR_WALL_TYPE_WEST" (Just $ IntegerLiteral 4),
            GlobalVar False False (JUserDefined "force") "bj_FORCE_ALL_PLAYERS" (Just NullLiteral)
          ] [] []),
      testCase "natives" $
        parseTestModule
          ("native function1 takes nothing returns nothing\n" ++
           "native function2 takes nothing returns widget\n" ++
           "native function3 takes integer par1, code par2 returns handle\n" ++
           "constant native function4 takes nothing returns nothing")
          (JassModule [] [] [
              NativeDecl False (FunctionDecl "function1" [] Nothing),
              NativeDecl False (FunctionDecl "function2" [] (Just $ JUserDefined "widget")),
              NativeDecl False (FunctionDecl "function3" [(JInteger, "par1"), (JCode, "par2")] (Just JHandle)),
              NativeDecl True (FunctionDecl "function4" [] Nothing)
            ] []),
      testCase "simple function" $
        parseTestModule
            ("function simple takes nothing returns nothing\n" ++
             "endfunction")
            (JassModule [] [] [] [
                Function False (FunctionDecl "simple" [] Nothing) [] []
            ]),
      testCase "constant function" $
        parseTestModule
            ("constant function simple takes nothing returns nothing\n" ++
             "endfunction")
            (JassModule [] [] [] [
                Function True (FunctionDecl "simple" [] Nothing) [] []
            ]),
      testCase "function with return type" $
        parseTestModule
            ("function simple takes nothing returns widget\n" ++
             "endfunction")
            (JassModule [] [] [] [
                Function False (FunctionDecl "simple" [] (Just (JUserDefined "widget"))) [] []
            ]),
      testCase "function with parameters" $
        parseTestModule
            ("function simple takes integer par1, real par2, handle par3 returns nothing\n" ++
             "endfunction")
            (JassModule [] [] [] [
                Function False (FunctionDecl "simple" [(JInteger, "par1"), (JReal, "par2"), (JHandle, "par3")] Nothing) [] []
            ]),
      testCase "function with parameters" $
        parseTestModule
            ("function simple takes widget par1, location par2, handle par3 returns nothing\n" ++
             "endfunction")
            (JassModule [] [] [] [
                Function False (FunctionDecl "simple" [(JUserDefined "widget", "par1"), (JUserDefined "location", "par2"), (JHandle, "par3")] Nothing) [] []
            ]),
      testCase "local variable" $
        parseTestLocalVar "local integer var1" 
          (LocalVar False JInteger "var1" Nothing),
      testCase "local variable" $
        parseTestLocalVar "local playercolor var1 = ConvertPlayerColor(0)" 
          (LocalVar False (JUserDefined "playercolor") "var1" (Just $ FunctionCall "ConvertPlayerColor" [IntegerLiteral 0])),
      testCase "local variable" $
        parseTestLocalVar "local widget var2 = null" 
          (LocalVar False (JUserDefined "widget") "var2" (Just NullLiteral)),
      testCase "local variable" $
        parseTestLocalVar "local real bj_PI = 3.14159" 
          (LocalVar False JReal "bj_PI" (Just $ RealLiteral 3.14159)),
      testCase "local variable" $
        parseTestLocalVar "local real bj_RADTODEG = 180.0/bj_PI" 
          (LocalVar False JReal "bj_RADTODEG" (Just $ BinaryExpression Divide (RealLiteral 180.0) (VariableReference "bj_PI"))),
      testCase "local variables" $
        parseTestFunction
            ("function simple takes nothing returns nothing\n" ++
             "local integer var1\n" ++
             "local real var2\n" ++
             "local integer var3 = 0\n" ++
             "local widget var4 = null\n" ++ 
             "local handle array var5\n" ++
             "endfunction")
            (Function False (FunctionDecl "simple" [] Nothing) [
                  LocalVar False JInteger "var1" Nothing,
                  LocalVar False JReal "var2" Nothing,
                  LocalVar False JInteger "var3" (Just $ IntegerLiteral 0),
                  LocalVar False (JUserDefined "widget") "var4" (Just NullLiteral),
                  LocalVar True JHandle "var5" Nothing
                ] []),
      testExpression "1+2"
            (BinaryExpression Summ (IntegerLiteral 1) (IntegerLiteral 2)),
      testExpression "1+2+3"
            (BinaryExpression Summ (IntegerLiteral 1) (BinaryExpression Summ (IntegerLiteral 2) (IntegerLiteral 3))),
      testExpression "1*2*3"
            (BinaryExpression Multiply (IntegerLiteral 1) (BinaryExpression Multiply (IntegerLiteral 2) (IntegerLiteral 3))),
      testExpression "1+2*3"
            (BinaryExpression Summ (IntegerLiteral 1) (BinaryExpression Multiply (IntegerLiteral 2) (IntegerLiteral 3))),
      testExpression "1*(2+2)"
            (BinaryExpression Multiply (IntegerLiteral 1) (BinaryExpression Summ (IntegerLiteral 2) (IntegerLiteral 2))),
      testExpression "1==2"
            (BinaryExpression Equal (IntegerLiteral 1) (IntegerLiteral 2)),
      testExpression "1 mod 2"
            (BinaryExpression Reminder (IntegerLiteral 1) (IntegerLiteral 2)),
      testExpression "1 > 2 == true"
            (BinaryExpression Equal (BinaryExpression Greater (IntegerLiteral 1) (IntegerLiteral 2)) (BoolLiteral True)),
      testExpression "not true == false"
            (BinaryExpression Equal (UnaryExpression Not (BoolLiteral True)) (BoolLiteral False)),
      testExpression "func()" (FunctionCall "func" []),
      testExpression "func(1,2)" (FunctionCall "func" [IntegerLiteral 1, IntegerLiteral 2]),
      testExpression "function func" (FunctionReference "func"),
      testExpression "arr[10]" (ArrayReference "arr" (IntegerLiteral 10)),
      testExpression "arr [10]" (ArrayReference "arr" (IntegerLiteral 10)),
      testExpression "variable" (VariableReference "variable"),
      testExpression "0xFF" (IntegerLiteral 255),
      testExpression "0x0F" (IntegerLiteral 15),
      testExpression "077" (IntegerLiteral 63),
      testExpression "0" (IntegerLiteral 0),
      testExpression "0.1" (RealLiteral 0.1),
      testExpression "-0.1" (RealLiteral (-0.1)),
      testExpression "+0.1" (RealLiteral 0.1),
      testExpression ".1" (RealLiteral 0.1),
      testExpression "-.1" (RealLiteral (-0.1)),
      testExpression "+.1" (RealLiteral 0.1),
      testExpression "1." (RealLiteral 1.0),
      testExpression "-1." (RealLiteral (-1.0)),
      testExpression "+1." (RealLiteral 1.0),
      testExpression "0.1E5" (RealLiteral 0.1e5),
      testExpression "0.1e5" (RealLiteral 0.1e5),
      testExpression "0.1e-5" (RealLiteral 0.1e-5),
      testExpression "0.1e+5" (RealLiteral 0.1e5),
      testExpression "+10.04e-10" (RealLiteral 10.04e-10),
      testExpression "-10.04e-10" (RealLiteral (-10.04e-10)),
      testExpression "'A000'" (IntegerLiteral (65*256^(3::Int) + 48*256^(2::Int) + 48*256 + 48)),
      testExpression "true" (BoolLiteral True),
      testExpression "false" (BoolLiteral False),
      testExpression "\"abs\"" (StringLiteral "abs"),
      testExpression "\"abs\\nabs\"" (StringLiteral "abs\nabs"),
      testExpression "1 - 2" (BinaryExpression Substract (IntegerLiteral 1) (IntegerLiteral 2)),
      testExpression "GetLocationY(locB) - GetLocationY(locA)"
        (BinaryExpression Substract 
          (FunctionCall "GetLocationY" [VariableReference "locB"]) 
          (FunctionCall "GetLocationY" [VariableReference "locA"])),
      testExpression "bj_RADTODEG * Atan2(GetLocationY(locB) - GetLocationY(locA), GetLocationX(locB) - GetLocationX(locA))" 
        (BinaryExpression Multiply (VariableReference "bj_RADTODEG")
            (FunctionCall "Atan2" [
              BinaryExpression Substract (FunctionCall "GetLocationY" [VariableReference "locB"]) (FunctionCall "GetLocationY" [VariableReference "locA"]),
              BinaryExpression Substract (FunctionCall "GetLocationX" [VariableReference "locB"]) (FunctionCall "GetLocationX" [VariableReference "locA"])
              ])),
      testStatement "set var = 0" (SetStatement False "var" (IntegerLiteral 0)),
      testStatement "set var[0] = 0" (SetArrayStatement False "var" (IntegerLiteral 0) (IntegerLiteral 0)),
      testStatement "call func()" (CallStatement False "func" []),
      testStatement "call func(var)" (CallStatement False "func" [VariableReference "var"]),
      testStatement "exitwhen true" (ExitWhenStatement (BoolLiteral True)),
      testStatement "return" (ReturnStatement Nothing),
      testStatement "return 42.0" (ReturnStatement (Just $ RealLiteral 42.0)),
      testStatement "debug set var = 0" (SetStatement True "var" (IntegerLiteral 0)),
      testStatement "loop endloop" (LoopStatement False []),
      testStatement "loop return endloop" (LoopStatement False [ReturnStatement Nothing]),
      testStatement "loop exitwhen true endloop" (LoopStatement False [ExitWhenStatement (BoolLiteral True)]),
      testStatement "if true then endif" (IfThenElseStatement False (BoolLiteral True) [] []),
      testStatement "if true then return else return endif" 
        (IfThenElseStatement False (BoolLiteral True) [ReturnStatement Nothing] [(Nothing,[ReturnStatement Nothing])]),
      testStatement "if true then return elseif false then return else return endif" 
        (IfThenElseStatement False (BoolLiteral True) [ReturnStatement Nothing] [(Just $ BoolLiteral False, [ReturnStatement Nothing]), (Nothing, [ReturnStatement Nothing])]),
      testStatement "debug loop set var = 0 endloop" (LoopStatement True [SetStatement True "var" (IntegerLiteral 0)]),
      testCase "function" $ parseTestFunction 
        ("function AngleBetweenPoints takes location locA, location locB returns real\n" ++
         "return bj_RADTODEG * Atan2(GetLocationY(locB) - GetLocationY(locA), GetLocationX(locB) - GetLocationX(locA))\n" ++
         "endfunction")
        (Function False (FunctionDecl "AngleBetweenPoints" [(JUserDefined "location", "locA"), (JUserDefined "location", "locB")] (Just JReal)) [] [
          ReturnStatement $ Just $ BinaryExpression Multiply (VariableReference "bj_RADTODEG")
            (FunctionCall "Atan2" [
              BinaryExpression Substract (FunctionCall "GetLocationY" [VariableReference "locB"]) (FunctionCall "GetLocationY" [VariableReference "locA"]),
              BinaryExpression Substract (FunctionCall "GetLocationX" [VariableReference "locB"]) (FunctionCall "GetLocationX" [VariableReference "locA"])
              ])
          ])
    ]