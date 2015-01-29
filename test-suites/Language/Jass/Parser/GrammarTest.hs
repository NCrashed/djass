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

testJassFile' :: FilePath -> JassModule -> Assertion
testJassFile' path testModule = do 
  res <- parseJassFile path
  case res of
    Left err -> assertFailure (show err)
    Right module' -> module' @?= testModule
        
commonParsing :: TestTree
commonParsing = testGroup "common.j and blizzard.j parsing"
  [ testCase "common.j" $ testJassFile "tests/common.j",
    testCase "blizzard.j" $ testJassFile "tests/blizzard.j"
  ]

nosrc :: SrcPos
nosrc = SrcPos "" 0 0

simpleParsing :: TestTree
simpleParsing = testGroup "hello world parsing"
  [ testCase "hello world" $ testJassFile' "tests/hello.j" (JassModule nosrc [] [] [
      NativeDecl nosrc False (FunctionDecl nosrc "writeln" [Parameter nosrc JString "msg"] Nothing)
    ] [
      Function nosrc False (FunctionDecl nosrc "main" [] Nothing) [] [
        CallStatement nosrc False "writeln" [StringLiteral nosrc "Hello, first standalone JASS program!"]
      ]
    ])
  ]
  
syntaxTests :: TestTree
syntaxTests = testGroup "Syntax tests"
    [ testCase "empty module" $ parseTestModule "" (JassModule nosrc [] [] [] []),
      testCase "Type declarations" $
        parseTestModule
            ("type widget extends handle\n" ++
            "type destructable extends widget")
            (JassModule nosrc [
                TypeDef nosrc "widget" JHandle,
                TypeDef nosrc "destructable" (JUserDefined "widget")
            ] [] [] []),
      testCase "global variable" $
        parseTestGlobalVar "force bj_FORCE_ALL_PLAYERS = null" 
          (GlobalVar nosrc False False (JUserDefined "force") "bj_FORCE_ALL_PLAYERS" (Just $ NullLiteral nosrc)),
      testCase "globals" $
        parseTestTwoGlobalVars
          ("constant integer   bj_ELEVATOR_WALL_TYPE_WEST       = 4\n" ++
           "force              bj_FORCE_ALL_PLAYERS        = null\n")
          (
            GlobalVar nosrc True False JInteger "bj_ELEVATOR_WALL_TYPE_WEST" (Just $ IntegerLiteral nosrc 4),
            GlobalVar nosrc False False (JUserDefined "force") "bj_FORCE_ALL_PLAYERS" (Just $ NullLiteral nosrc)
          ),
      testExpression "ConvertPlayerColor(0)"
        (FunctionCall nosrc "ConvertPlayerColor" [IntegerLiteral nosrc 0]),
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
              "constant real      bj_PI                            = 3.14159\n" ++
              "force              bj_FORCE_ALL_PLAYERS        = null\n" ++
           "endglobals")
          (JassModule nosrc [] [
            GlobalVar nosrc False False JReal "global1" Nothing,
            GlobalVar nosrc False False JInteger "global2" (Just $ IntegerLiteral nosrc 0),
            GlobalVar nosrc True False JReal "global3" (Just $ RealLiteral nosrc 3.14),
            GlobalVar nosrc False True JHandle "global4" Nothing,
            GlobalVar nosrc True True (JUserDefined "widget") "global5" Nothing,
            GlobalVar nosrc True False (JUserDefined "playercolor") "global6" (Just $ FunctionCall nosrc "ConvertPlayerColor" [IntegerLiteral nosrc 0]),
            GlobalVar nosrc True False JInteger "bj_ELEVATOR_WALL_TYPE_WEST" (Just $ IntegerLiteral nosrc 4),
            GlobalVar nosrc True False JReal "bj_PI" (Just $ RealLiteral nosrc 3.14159),
            GlobalVar nosrc False False (JUserDefined "force") "bj_FORCE_ALL_PLAYERS" (Just $ NullLiteral nosrc)
          ] [] []),
      testCase "natives" $
        parseTestModule
          ("native function1 takes nothing returns nothing\n" ++
           "native function2 takes nothing returns widget\n" ++
           "native function3 takes integer par1, code par2 returns handle\n" ++
           "constant native function4 takes nothing returns nothing")
          (JassModule nosrc [] [] [
              NativeDecl nosrc False (FunctionDecl nosrc "function1" [] Nothing),
              NativeDecl nosrc False (FunctionDecl nosrc "function2" [] (Just $ JUserDefined "widget")),
              NativeDecl nosrc False (FunctionDecl nosrc "function3" [
                Parameter nosrc JInteger "par1", 
                Parameter nosrc JCode "par2"] (Just JHandle)),
              NativeDecl nosrc True (FunctionDecl nosrc "function4" [] Nothing)
            ] []),
      testCase "simple function" $
        parseTestModule
            ("function simple takes nothing returns nothing\n" ++
             "endfunction")
            (JassModule nosrc [] [] [] [
                Function nosrc False (FunctionDecl nosrc "simple" [] Nothing) [] []
            ]),
      testCase "constant function" $
        parseTestModule
            ("constant function simple takes nothing returns nothing\n" ++
             "endfunction")
            (JassModule nosrc [] [] [] [
                Function nosrc True (FunctionDecl nosrc "simple" [] Nothing) [] []
            ]),
      testCase "function with return type" $
        parseTestModule
            ("function simple takes nothing returns widget\n" ++
             "endfunction")
            (JassModule nosrc [] [] [] [
                Function nosrc False (FunctionDecl nosrc "simple" [] (Just (JUserDefined "widget"))) [] []
            ]),
      testCase "function with parameters" $
        parseTestModule
            ("function simple takes integer par1, real par2, handle par3 returns nothing\n" ++
             "endfunction")
            (JassModule nosrc [] [] [] [
                Function nosrc False (FunctionDecl nosrc "simple" [Parameter nosrc JInteger "par1", Parameter nosrc JReal "par2", Parameter nosrc JHandle "par3"] Nothing) [] []
            ]),
      testCase "function with parameters" $
        parseTestModule
            ("function simple takes widget par1, location par2, handle par3 returns nothing\n" ++
             "endfunction")
            (JassModule nosrc [] [] [] [
                Function nosrc False (FunctionDecl nosrc "simple" [
                  Parameter nosrc (JUserDefined "widget") "par1", 
                  Parameter nosrc (JUserDefined "location") "par2", 
                  Parameter nosrc JHandle "par3"] 
                Nothing) [] []
            ]),
      testCase "local variable" $
        parseTestLocalVar "local integer var1" 
          (LocalVar nosrc False JInteger "var1" Nothing),
      testCase "local variable" $
        parseTestLocalVar "local playercolor var1 = ConvertPlayerColor(0)" 
          (LocalVar nosrc False (JUserDefined "playercolor") "var1" (Just $ FunctionCall nosrc "ConvertPlayerColor" [IntegerLiteral nosrc 0])),
      testCase "local variable" $
        parseTestLocalVar "local widget var2 = null" 
          (LocalVar nosrc False (JUserDefined "widget") "var2" (Just $ NullLiteral nosrc)),
      testCase "local variable" $
        parseTestLocalVar "local real bj_PI = 3.14159" 
          (LocalVar nosrc False JReal "bj_PI" (Just $ RealLiteral nosrc 3.14159)),
      testCase "local variable" $
        parseTestLocalVar "local real bj_RADTODEG = 180.0/bj_PI" 
          (LocalVar nosrc False JReal "bj_RADTODEG" (Just $ BinaryExpression nosrc Divide (RealLiteral nosrc 180.0) (VariableReference nosrc "bj_PI"))),
      testCase "local variables" $
        parseTestFunction
            ("function simple takes nothing returns nothing\n" ++
             "local integer var1\n" ++
             "local real var2\n" ++
             "local integer var3 = 0\n" ++
             "local widget var4 = null\n" ++ 
             "local handle array var5\n" ++
             "endfunction")
            (Function nosrc False (FunctionDecl nosrc "simple" [] Nothing) [
                  LocalVar nosrc False JInteger "var1" Nothing,
                  LocalVar nosrc False JReal "var2" Nothing,
                  LocalVar nosrc False JInteger "var3" (Just $ IntegerLiteral nosrc 0),
                  LocalVar nosrc False (JUserDefined "widget") "var4" (Just $ NullLiteral nosrc),
                  LocalVar nosrc True JHandle "var5" Nothing
                ] []),
      testExpression "1+2"
            (BinaryExpression nosrc Summ (IntegerLiteral nosrc 1) (IntegerLiteral nosrc 2)),
      testExpression "1+2+3"
            (BinaryExpression nosrc Summ (IntegerLiteral nosrc 1) (BinaryExpression nosrc Summ (IntegerLiteral nosrc 2) (IntegerLiteral nosrc 3))),
      testExpression "1*2*3"
            (BinaryExpression nosrc Multiply (IntegerLiteral nosrc 1) (BinaryExpression nosrc Multiply (IntegerLiteral nosrc 2) (IntegerLiteral nosrc 3))),
      testExpression "1+2*3"
            (BinaryExpression nosrc Summ (IntegerLiteral nosrc 1) (BinaryExpression nosrc Multiply (IntegerLiteral nosrc 2) (IntegerLiteral nosrc 3))),
      testExpression "1*(2+2)"
            (BinaryExpression nosrc Multiply (IntegerLiteral nosrc 1) (BinaryExpression nosrc Summ (IntegerLiteral nosrc 2) (IntegerLiteral nosrc 2))),
      testExpression "1==2"
            (BinaryExpression nosrc Equal (IntegerLiteral nosrc 1) (IntegerLiteral nosrc 2)),
      testExpression "1 mod 2"
            (BinaryExpression nosrc Reminder (IntegerLiteral nosrc 1) (IntegerLiteral nosrc 2)),
      testExpression "1 > 2 == true"
            (BinaryExpression nosrc Equal (BinaryExpression nosrc Greater (IntegerLiteral nosrc 1) (IntegerLiteral nosrc 2)) (BoolLiteral nosrc True)),
      testExpression "not true == false"
            (BinaryExpression nosrc Equal (UnaryExpression nosrc Not (BoolLiteral nosrc True)) (BoolLiteral nosrc False)),
      testExpression "func()" (FunctionCall nosrc "func" []),
      testExpression "func(1,2)" (FunctionCall nosrc "func" [IntegerLiteral nosrc 1, IntegerLiteral nosrc 2]),
      testExpression "function func" (FunctionReference nosrc "func"),
      testExpression "arr[10]" (ArrayReference nosrc "arr" (IntegerLiteral nosrc 10)),
      testExpression "arr [10]" (ArrayReference nosrc "arr" (IntegerLiteral nosrc 10)),
      testExpression "variable" (VariableReference nosrc "variable"),
      testExpression "0xFF" (IntegerLiteral nosrc 255),
      testExpression "0x0F" (IntegerLiteral nosrc 15),
      testExpression "077" (IntegerLiteral nosrc 63),
      testExpression "0" (IntegerLiteral nosrc 0),
      testExpression "0.1" (RealLiteral nosrc 0.1),
      testExpression "-0.1" (RealLiteral nosrc (-0.1)),
      testExpression "+0.1" (RealLiteral nosrc 0.1),
      testExpression ".1" (RealLiteral nosrc 0.1),
      testExpression "-.1" (RealLiteral nosrc (-0.1)),
      testExpression "+.1" (RealLiteral nosrc 0.1),
      testExpression "1." (RealLiteral nosrc 1.0),
      testExpression "-1." (RealLiteral nosrc (-1.0)),
      testExpression "+1." (RealLiteral nosrc 1.0),
      testExpression "0.1E5" (RealLiteral nosrc 0.1e5),
      testExpression "0.1e5" (RealLiteral nosrc 0.1e5),
      testExpression "0.1e-5" (RealLiteral nosrc 0.1e-5),
      testExpression "0.1e+5" (RealLiteral nosrc 0.1e5),
      testExpression "+10.04e-10" (RealLiteral nosrc 10.04e-10),
      testExpression "-10.04e-10" (RealLiteral nosrc (-10.04e-10)),
      testExpression "'A000'" (IntegerLiteral nosrc (65*256^(3::Int) + 48*256^(2::Int) + 48*256 + 48)),
      testExpression "true" (BoolLiteral nosrc True),
      testExpression "false" (BoolLiteral nosrc False),
      testExpression "\"abs\"" (StringLiteral nosrc "abs"),
      testExpression "\"abs\\nabs\"" (StringLiteral nosrc "abs\nabs"),
      testExpression "1 - 2" (BinaryExpression nosrc Substract (IntegerLiteral nosrc 1) (IntegerLiteral nosrc 2)),
      testExpression "GetLocationY(locB) - GetLocationY(locA)"
        (BinaryExpression nosrc Substract 
          (FunctionCall nosrc "GetLocationY" [VariableReference nosrc "locB"]) 
          (FunctionCall nosrc "GetLocationY" [VariableReference nosrc "locA"])),
      testExpression "bj_RADTODEG * Atan2(GetLocationY(locB) - GetLocationY(locA), GetLocationX(locB) - GetLocationX(locA))" 
        (BinaryExpression nosrc Multiply (VariableReference nosrc "bj_RADTODEG")
            (FunctionCall nosrc "Atan2" [
              BinaryExpression nosrc Substract (FunctionCall nosrc "GetLocationY" [VariableReference nosrc "locB"]) (FunctionCall nosrc "GetLocationY" [VariableReference nosrc "locA"]),
              BinaryExpression nosrc Substract (FunctionCall nosrc "GetLocationX" [VariableReference nosrc "locB"]) (FunctionCall nosrc "GetLocationX" [VariableReference nosrc "locA"])
              ])),
      testStatement "set var = 0" (SetStatement nosrc False "var" (IntegerLiteral nosrc 0)),
      testStatement "set var[0] = 0" (SetArrayStatement nosrc False "var" (IntegerLiteral nosrc 0) (IntegerLiteral nosrc 0)),
      testStatement "call func()" (CallStatement nosrc False "func" []),
      testStatement "call func(var)" (CallStatement nosrc False "func" [VariableReference nosrc "var"]),
      testStatement "exitwhen true" (ExitWhenStatement nosrc (BoolLiteral nosrc True)),
      testStatement "return" (ReturnStatement nosrc Nothing),
      testStatement "return 42.0" (ReturnStatement nosrc (Just $ RealLiteral nosrc 42.0)),
      testStatement "debug set var = 0" (SetStatement nosrc True "var" (IntegerLiteral nosrc 0)),
      testStatement "loop endloop" (LoopStatement nosrc False []),
      testStatement "loop return endloop" (LoopStatement nosrc False [ReturnStatement nosrc Nothing]),
      testStatement "loop exitwhen true endloop" (LoopStatement nosrc False [ExitWhenStatement nosrc (BoolLiteral nosrc True)]),
      testStatement "if true then endif" (IfThenElseStatement nosrc False (BoolLiteral nosrc True) [] []),
      testStatement "if true then return else return endif" 
        (IfThenElseStatement nosrc False (BoolLiteral nosrc True) [ReturnStatement nosrc Nothing] [(Nothing,[ReturnStatement nosrc Nothing])]),
      testStatement "if true then return elseif false then return else return endif" 
        (IfThenElseStatement nosrc False (BoolLiteral nosrc True) [ReturnStatement nosrc Nothing] [(Just $ BoolLiteral nosrc False, [ReturnStatement nosrc Nothing]), (Nothing, [ReturnStatement nosrc Nothing])]),
      testStatement "debug loop set var = 0 endloop" (LoopStatement nosrc True [SetStatement nosrc True "var" (IntegerLiteral nosrc 0)]),
      testCase "function" $ parseTestFunction 
        ("function AngleBetweenPoints takes location locA, location locB returns real\n" ++
         "return bj_RADTODEG * Atan2(GetLocationY(locB) - GetLocationY(locA), GetLocationX(locB) - GetLocationX(locA))\n" ++
         "endfunction")
        (Function nosrc False (FunctionDecl nosrc "AngleBetweenPoints" [
            Parameter nosrc (JUserDefined "location") "locA", 
            Parameter nosrc (JUserDefined "location") "locB"] 
          (Just JReal)) [] [
            ReturnStatement nosrc $ Just $ BinaryExpression nosrc Multiply (VariableReference nosrc "bj_RADTODEG")
              (FunctionCall nosrc "Atan2" [
                BinaryExpression nosrc Substract (FunctionCall nosrc "GetLocationY" [VariableReference nosrc "locB"]) (FunctionCall nosrc "GetLocationY" [VariableReference nosrc "locA"]),
                BinaryExpression nosrc Substract (FunctionCall nosrc "GetLocationX" [VariableReference nosrc "locB"]) (FunctionCall nosrc "GetLocationX" [VariableReference nosrc "locA"])
                ])
          ])
    ]
