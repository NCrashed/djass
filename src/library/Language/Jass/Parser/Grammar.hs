{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Language.Jass.Parser.Grammar(
    parseJass,
    parseJassFile,
    parseJassFunction,
    parseJassLocalVar,
    parseJassGlobalVar,
    parseJassTwoGlobalVars,
    parseJassGlobalVars,
    parseJassTypeDef,
    parseJassNative,
    parseJassExpression,
    parseJassStatement
    ) where
    
import Text.Peggy hiding(space)
import Data.Digits
import Data.Char
import Data.Maybe
import Language.Jass.Parser.AST as AST

data Trio a b c = TrioFirst a | TrioSecond b | TrioThird c
 
[peggy|
    jassModule :: JassModule = getPos (typeDef+ {TrioFirst $1}/ globalVars {TrioSecond $1}/ nativeDecl+ {TrioThird $1})* function* {parseJassModule $1 $2 $3}

    lowerCase :: Char = [a-z]
    upperCase :: Char = [A-Z]
    digit :: Char = [0-9]
    alpha :: Char = lowerCase / upperCase / '_' {'_'}
    alphanum :: Char = alpha / digit
    doublequote :: Char = '\"' {'\"'}
    quote :: Char = '\'' {'\''}
    backslash :: Char = '\\' {'\\'}
    
    comment :: () = lineComment / blockComment
    lineComment :: () = '//' (!'\n' .)* '\n' { () }
    blockComment :: () = '/*' (!'*/' . { () })* '*/' { () }
    space :: () = [ \t\r\n] { () } / comment
    
    keyword :: () = "type" / "extends" / "null" / "true" / "false" / "function" / "constant"
        / "mod" / "and" / "or" / "not" / "native" / "returns" / "take" / "globals" / "endglobals"
        / "nothing" / "native" / "endfunction" / "local" / "array"
        / "set" / "call" / "if" / "then" / "endif" / "elseif" / "else"
        / "loop" / "endloop" / "exitwhen" / "return" / "debug"
    identifier :: String = !keyword alpha alphanum* {[$1] ++ $2}
    
    jassType :: JassType = "integer" {JInteger} / "real" {JReal} / "boolean" {JBoolean} / "string" {JString} / "handle" {JHandle} / "code" {JCode} / identifier {JUserDefined $1}
    typeDef :: TypeDef = getPos "type" identifier "extends" jassType {TypeDef $1 $2 $3}
   
    escapeSequence :: Char = backslash (quote / doublequote / backslash 
      / 'a' {'\a'} / 'b' {'\b'} / 'f' {'\f'} / 'n' {'\n'} / 'r' {'\r'} / 't' {'\t'} / 'v' {'\v'}) { $2 }
    dqchar :: Char = escapeSequence / !doublequote .
    stringLiteral :: Expression = getPos doublequote (dqchar)* doublequote {StringLiteral $1 $3}
    
    integer :: String = digit+
    sign :: Char = [-] / [+]
    decimal :: Int = [1-9][0-9]* {parseDecimal ([$1] ++ $2)}
    octal :: Int = '0'[0-7]* {if null $1 then 0 else parseOctal $1}
    hex :: Int = '$'[0-9a-fA-F]+ {parseHex $1} / '0'[xX][0-9a-fA-F]+ {parseHex $2}
    rawcode :: Int = quote . . . . quote {parseRawCode [$2,$3,$4,$5]}
    
    intLiteral :: Expression = getPos sign? (decimal / hex / octal / rawcode) {IntegerLiteral $1 (parseIntLiteral $2 $3)}
    realLiteral :: Expression = getPos sign? integer? '.' integer? (('e' / 'E') sign? integer {($2, $3)})? {RealLiteral $1 (parseRealLiteral $2 $3 $4 $5)}
    boolLiteral :: Expression = getPos "true" {BoolLiteral $1 True} / getPos "false" {BoolLiteral $1 False}
    nullLiteral :: Expression = getPos "null" {NullLiteral $1}
    literal :: Expression = realLiteral / intLiteral / boolLiteral / nullLiteral / stringLiteral
    
    agrumentsList :: [Expression] = expression ("," expression)* {[$1] ++ $2}
    
    parens :: Expression = "(" expression ")"
    funcRef :: Expression = getPos "function" identifier {FunctionReference $1 $2}
    arrayRef :: Expression = getPos identifier "[" expression "]" {ArrayReference $1 $2 $3}
    funcCall :: Expression = getPos identifier "(" agrumentsList? ")" {FunctionCall $1 $2 (fromMaybe [] $3)}
    varRef :: Expression = getPos identifier {VariableReference $1 $2}
    
    primaryExpression :: Expression = literal / varRef / parens
    postfixExpression :: Expression = arrayRef / funcCall / funcRef / primaryExpression
    
    notOp :: UnaryOperator = "not" {AST.Not}
    plus :: UnaryOperator = "+" {Plus}
    negation :: UnaryOperator = "-" {Negation}
    unaryExpression :: Expression = postfixExpression / getPos (plus / negation / notOp) unaryExpression {UnaryExpression $1 $2 $3}
    
    summ :: BinaryOperator = "+" {Summ}
    subs :: BinaryOperator = "-" {Substract}
    mult :: BinaryOperator = "*" {Multiply}
    divOp :: BinaryOperator = "/" {Divide}
    modOp :: BinaryOperator = "mod" {Reminder}
    greaterEqual :: BinaryOperator = ">=" {GreaterEqual}
    lessEqual :: BinaryOperator = "<=" {LessEqual}
    less :: BinaryOperator = "<" {Less}
    greater :: BinaryOperator = ">" {Greater}
    equal :: BinaryOperator = "==" {Equal}
    notEqual :: BinaryOperator = "!=" {NotEqual}    
    andOp :: BinaryOperator = "and" {AST.And}
    orOp :: BinaryOperator = "or" {AST.Or}
    
    multiplicativeExpression :: Expression = getPos unaryExpression ((mult / divOp / modOp) multiplicativeExpression {($1, $2)})? {parseBinaryExpression $1 $2 $3}
    additiveExpression :: Expression = getPos multiplicativeExpression ((summ / subs) additiveExpression {($1, $2)})? {parseBinaryExpression $1 $2 $3}
    relationalExpression :: Expression = getPos additiveExpression ((greaterEqual / lessEqual / greater / less) relationalExpression {($1, $2)})? {parseBinaryExpression $1 $2 $3}
    equalityExpression :: Expression = getPos relationalExpression ((equal / notEqual) equalityExpression {($1, $2)})? {parseBinaryExpression $1 $2 $3}
    andExpression :: Expression = getPos equalityExpression (andOp andExpression {($1, $2)})? {parseBinaryExpression $1 $2 $3}
    orExpression :: Expression = getPos andExpression (orOp orExpression {($1, $2)})? {parseBinaryExpression $1 $2 $3}
    expression :: Expression = orExpression
    
    constant :: () = "constant" {()}
    -- Additional space* is a workaround around bug in peggy
    podVarDecl :: GlobalVar = getPos constant? space* jassType space* identifier ("=" expression)? {GlobalVar $1 (isJust $2) False $4 $6 $7}
    arrayVarDecl :: GlobalVar = getPos constant? space* jassType "array" identifier {GlobalVar $1 (isJust $2) True $4 $5 Nothing}
    globalVar :: GlobalVar = podVarDecl / arrayVarDecl
    globalVars :: [GlobalVar] = "globals" globalVar* "endglobals"
    
    -- Additional space* is a workaround around bug in peggy
    param :: Parameter = getPos jassType space* identifier {Parameter $1 $2 $4}
    paramList :: [Parameter] = param ("," param)* {[$1] ++ $2}
    functionDecl :: FunctionDecl = getPos identifier "takes" ("nothing" {[]} / paramList) "returns" (jassType {Just $1} / "nothing" {Nothing}) {FunctionDecl $1 $2 $3 $4}
    nativeDecl :: NativeDecl = getPos constant? "native" functionDecl {NativeDecl $1 (isJust $2) $3}
    function :: Function = getPos constant? "function" functionDecl localVarList statementList "endfunction" {Function $1 (isJust $2) $3 $4 $5}
    
    localVarList :: [LocalVar] = localVar*
    -- Additional space* is a workaround around bug in peggy
    podLocalVar :: LocalVar = space* getPos jassType space* identifier ("=" expression)? {LocalVar $2 False $3 $5 $6}
    arrayLocalVar :: LocalVar =  space* getPos jassType "array" identifier {LocalVar $2 True $3 $4 Nothing}
    localVar :: LocalVar = "local" (podLocalVar / arrayLocalVar)
    
    statementList :: [Statement] = statement*
    statement :: Statement = setStatement / callStatement / ifThenElseStatement / loopStatement / exitWhenStatement / returnStatement / debugStatement
    setStatement :: Statement = getPos "set" identifier "=" expression {SetStatement $1 False $2 $3} / getPos "set" identifier "[" expression "]" "=" expression {SetArrayStatement $1 False $2 $3 $4}
    ifThenElseStatement :: Statement = getPos "if" expression "then" statementList elseClause? "endif" {IfThenElseStatement $1 False $2 $3 (fromMaybe [] $4)}
    elseClause :: [(Maybe Expression, [Statement])] = "elseif" expression "then" statementList elseClause? {[(Just $1, $2)] ++ fromMaybe [] $3} / "else" statementList {[(Nothing, $1)]}
    callStatement :: Statement = getPos "call" identifier "(" agrumentsList? ")" {CallStatement $1 False $2 (fromMaybe [] $3)}
    loopStatement :: Statement = getPos "loop" statementList "endloop" {LoopStatement $1 False $2}
    exitWhenStatement :: Statement = getPos "exitwhen" expression {ExitWhenStatement $1 $2}
    returnStatement :: Statement = getPos "return" expression? {ReturnStatement $1 $2}
    debugStatement :: Statement = "debug" (setStatement / callStatement / ifThenElseStatement / loopStatement) {setDebugStatement True $1}

    -- DEBUG
    twoGlobals :: (GlobalVar, GlobalVar) = globalVar globalVar
|]

parseDecimal :: String -> Int
parseDecimal = unDigits 10 .map (\a->read [a])

parseOctal :: String -> Int
parseOctal = unDigits 8 . map (\a->read [a])

parseHex :: String -> Int
parseHex = unDigits 16 . map hexDigit
    where 
        hexDigit :: Char -> Int
        hexDigit '0' = 0
        hexDigit '1' = 1
        hexDigit '2' = 2
        hexDigit '3' = 3
        hexDigit '4' = 4
        hexDigit '5' = 5
        hexDigit '6' = 6
        hexDigit '7' = 7
        hexDigit '8' = 8
        hexDigit '9' = 9
        hexDigit 'a' = 10
        hexDigit 'A' = 10
        hexDigit 'b' = 11
        hexDigit 'B' = 11
        hexDigit 'c' = 12
        hexDigit 'C' = 12
        hexDigit 'd' = 13
        hexDigit 'D' = 13
        hexDigit 'e' = 14
        hexDigit 'E' = 14
        hexDigit 'f' = 15
        hexDigit 'F' = 15
        hexDigit hdigit = error $ "undefined hex digit " ++ show hdigit
        
parseRawCode :: String -> Int
parseRawCode s = unDigits 256 $ map ord s

parseIntLiteral :: Maybe Char -> Int -> Int
parseIntLiteral (Just '-') i = -i
parseIntLiteral _ i = i

parseRealLiteral :: Maybe Char -> Maybe String -> Maybe String -> Maybe (Maybe Char, String) -> Float
parseRealLiteral csign intPart decPart expPart = read $ convSign csign ++ fromMaybe "0" intPart ++ "." ++ fromMaybe "0" decPart ++ expPartStr expPart
    where
        convSign :: Maybe Char -> String
        convSign (Just '-') = "-"
        convSign _ = ""
        
        expPartStr :: Maybe (Maybe Char, String) -> String
        expPartStr (Just (esign, emantis)) = "e" ++ convSign esign ++ emantis
        expPartStr _ = ""
        
parseBinaryExpression :: SrcPos -> Expression -> Maybe (BinaryOperator, Expression) -> Expression
parseBinaryExpression _ left Nothing = left
parseBinaryExpression pos left (Just (op, right)) = BinaryExpression pos op left right

parseJassModule :: SrcPos -> [Trio [TypeDef] [GlobalVar] [NativeDecl]] -> [Function] -> JassModule
parseJassModule pos trios functions = let (tdefs, gvars, natives) = foldl accTrios ([], [], []) trios 
  in JassModule pos tdefs gvars natives functions
  where 
      accTrios (tdefs, gvars, natives) (TrioFirst tdefs') = (tdefs ++ tdefs', gvars, natives)
      accTrios (tdefs, gvars, natives) (TrioSecond gvars') = (tdefs, gvars++gvars', natives)
      accTrios (tdefs, gvars, natives) (TrioThird natives') = (tdefs, gvars, natives++natives')

-- | Parsing jass source
parseJass :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError JassModule -- ^ Error or AST
parseJass = parseString jassModule

-- | Parsing jass grammar subset 
parseJassFunction :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError Function -- ^ Error or AST
parseJassFunction = parseString function

-- | Parsing jass grammar subset 
parseJassLocalVar :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError LocalVar -- ^ Error or AST
parseJassLocalVar = parseString localVar

-- | Parsing jass grammar subset 
parseJassGlobalVar :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError GlobalVar -- ^ Error or AST
parseJassGlobalVar = parseString globalVar

-- | Parsing jass grammar subset (debug function)
parseJassTwoGlobalVars :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError (GlobalVar, GlobalVar) -- ^ Error or AST
parseJassTwoGlobalVars = parseString twoGlobals

-- | Parsing jass grammar subset 
parseJassGlobalVars :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError [GlobalVar] -- ^ Error or AST
parseJassGlobalVars = parseString globalVars

-- | Parsing jass grammar subset 
parseJassTypeDef :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError TypeDef -- ^ Error or AST
parseJassTypeDef = parseString typeDef

-- | Parsing jass grammar subset 
parseJassNative :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError NativeDecl -- ^ Error or AST
parseJassNative = parseString nativeDecl

-- | Parsing jass grammar subset 
parseJassExpression :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError Expression -- ^ Error or AST
parseJassExpression = parseString expression

-- | Parsing jass grammar subset 
parseJassStatement :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError Statement -- ^ Error or AST
parseJassStatement = parseString statement

-- | Parsing jass source from file
parseJassFile :: FilePath -- ^ File path with jass source 
    -> IO (Either ParseError JassModule) -- ^ Error or AST
parseJassFile = parseFile jassModule        