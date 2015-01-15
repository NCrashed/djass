{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Language.Jass.Parser.Grammar(
    parseJass,
    parseJassFile
    ) where
    
import Text.Peggy hiding(space)
import Data.Digits
import Data.Char
import Data.Maybe
import Language.Jass.Parser.AST as AST

data Trio a b c = TrioFirst a | TrioSecond b | TrioThird c
 
[peggy|
    jassModule :: JassModule = (typeDef+ {TrioFirst $1}/ globalVars {TrioSecond $1}/ nativeDecl+ {TrioThird $1})* function* {parseJassModule $1 $2}

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
        / "set" / "call" / "if" / "then" / "endif" / "elseif"
        / "loop" / "endloop" / "exitwhen" / "return" / "debug"
    identifier :: String = !keyword alpha alphanum* {[$1] ++ $2}
    
    jassType :: JassType = "integer" {JInteger} / "real" {JReal} / "boolean" {JBoolean} / "string" {JString} / "handle" {JHandle} / "code" {JCode} / identifier {JUserDefined $1}
    typeDef :: TypeDef = "type" identifier "extends" jassType {TypeDef $1 $2}
   
    escapeSequence :: Char = backslash (quote / doublequote / backslash / [abfnrtv]) { $2 }
    dqchar :: Char = escapeSequence / !doublequote .
    stringLiteral :: Expression = doublequote (dqchar)* doublequote {StringLiteral $2}
    
    integer :: Int = digit+ {read $1}
    sign :: Char = [-] / [+]
    decimal :: Int = [1-9][0-9]* {parseDecimal ([$1] ++ $2)}
    octal :: Int = '0'[0-7]* {parseOctal $1}
    hex :: Int = '$'[0-9a-fA-F]+ {parseHex $1} / '0'[xX][0-9a-fA-F]+ {parseHex $2}
    rawcode :: Int = quote . . . . quote {parseRawCode [$2,$3,$4,$5]}
    
    intLiteral :: Expression = sign? (decimal / hex / octal / rawcode) {IntegerLiteral (parseIntLiteral $1 $2)}
    realLiteral :: Expression = sign? integer? '.' integer? (('e' / 'E') sign? integer {($2, $3)})? {RealLiteral (parseRealLiteral $1 $2 $3 $4)}
    boolLiteral :: Expression = "true" {BoolLiteral True} / "false" {BoolLiteral False}
    nullLiteral :: Expression = "null" {NullLiteral}
    literal :: Expression = realLiteral / intLiteral / boolLiteral / nullLiteral / stringLiteral
    
    agrumentsList :: [Expression] = expression ("," expression)* {[$1] ++ $2}
    
    parens :: Expression = "(" expression ")"
    funcRef :: Expression = "function" identifier {FunctionReference $1}
    arrayRef :: Expression = identifier "[" expression "]" {ArrayReference $1 $2}
    funcCall :: Expression = identifier "(" agrumentsList? ")" {FunctionCall $1 (fromMaybe [] $2)}
    varRef :: Expression = identifier {VariableReference $1}
    
    primaryExpression :: Expression = literal / varRef / parens
    postfixExpression :: Expression = arrayRef / funcCall / funcRef / primaryExpression
    
    notOp :: UnaryOperator = "not" {AST.Not}
    plus :: UnaryOperator = "+" {Plus}
    negation :: UnaryOperator = "-" {Negation}
    unaryExpression :: Expression = postfixExpression / (plus / negation / notOp) expression {UnaryExpression $1 $2}
    
    summ :: BinaryOperator = "+" {Summ}
    subs :: BinaryOperator = "+" {Substract}
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
    
    multiplicativeExpression :: Expression = unaryExpression ((mult / divOp / modOp) multiplicativeExpression {($1, $2)})? {parseBinaryExpression $1 $2}
    additiveExpression :: Expression = multiplicativeExpression ((summ / subs) additiveExpression {($1, $2)})? {parseBinaryExpression $1 $2}
    relationalExpression :: Expression = additiveExpression ((greaterEqual / lessEqual / greater / less) relationalExpression {($1, $2)})? {parseBinaryExpression $1 $2}
    equalityExpression :: Expression = relationalExpression ((equal / notEqual) equalityExpression {($1, $2)})? {parseBinaryExpression $1 $2}
    andExpression :: Expression = equalityExpression (andOp andExpression {($1, $2)})? {parseBinaryExpression $1 $2}
    orExpression :: Expression = andExpression (orOp orExpression {($1, $2)})? {parseBinaryExpression $1 $2}
    expression :: Expression = orExpression
    
    constant :: () = "constant" {()}
    podVarDecl :: GlobalVar = constant? jassType identifier ("=" expression)? {GlobalVar (isJust $1) False $2 $3 $4}
    arrayVarDecl :: GlobalVar = constant? jassType "array" identifier {GlobalVar (isJust $1) True $2 $3 Nothing}
    globalVar :: GlobalVar = podVarDecl / arrayVarDecl
    globalVars :: [GlobalVar] = "globals" globalVar* "endglobals"
    
    param :: (JassType, Name) = jassType identifier {($1, $2)}
    paramList :: [(JassType, Name)] = param ("," param)* {[$1] ++ $2}
    functionDecl :: FunctionDecl = identifier "takes" ("nothing" {[]} / paramList) "returns" (jassType {Just $1} / "nothing" {Nothing}) {FunctionDecl $1 $2 $3}
    nativeDecl :: NativeDecl = constant? "native" functionDecl {NativeDecl (isJust $1) $2}
    function :: Function = constant? "function" functionDecl localVarList statementList "endfunction" {Function (isJust $1) $2 $3 $4}
    
    localVarList :: [LocalVar] = localVar*
    podLocalVar :: LocalVar = jassType identifier ("=" expression)? {LocalVar False $1 $2 $3}
    arrayLocalVar :: LocalVar = jassType "array" identifier {LocalVar True $1 $2 Nothing}
    localVar :: LocalVar = podLocalVar / arrayLocalVar
    
    statementList :: [Statement] = statement*
    statement :: Statement = setStatement / callStatement / ifThenElseStatement / loopStatement / exitWhenStatement / returnStatement / debugStatement
    setStatement :: Statement = "set" identifier "=" expression {SetStatement False $1 $2} / "set" identifier "[" expression "]" "=" expression {SetArrayStatement False $1 $2 $3}
    ifThenElseStatement :: Statement = "if" expression "then" statementList elseClause? "endif" {IfThenElseStatement False $1 $2 (fromMaybe [] $3)}
    elseClause :: [(Maybe Expression, [Statement])] = "elseif" expression "then" statementList elseClause? {[(Just $1, $2)] ++ fromMaybe [] $3} / "else" statementList {[(Nothing, $1)]}
    callStatement :: Statement = "call" identifier "(" agrumentsList? ")" {CallStatement False $1 (fromMaybe [] $2)}
    loopStatement :: Statement = "loop" statementList "endloop" {LoopStatement False $1}
    exitWhenStatement :: Statement = "exitwhen" expression {ExitWhenStatement $1}
    returnStatement :: Statement = "return" expression? {ReturnStatement $1}
    debugStatement :: Statement = "debug" (setStatement / callStatement / ifThenElseStatement / loopStatement) {setDebugStatement True $1}
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

parseRealLiteral :: Maybe Char -> Maybe Int -> Maybe Int -> Maybe (Maybe Char, Int) -> Float
parseRealLiteral csign intPart decPart expPart = read $ convSign csign ++ show (fromMaybe 0 intPart) ++ "." ++ show (fromMaybe 0 decPart) ++ expPartStr expPart
    where
        convSign :: Maybe Char -> String
        convSign (Just '-') = "-"
        convSign _ = "+"
        
        expPartStr :: Maybe (Maybe Char, Int) -> String
        expPartStr (Just (esign, emantis)) = convSign esign ++ show emantis
        expPartStr _ = ""
        
parseBinaryExpression :: Expression -> Maybe (BinaryOperator, Expression) -> Expression
parseBinaryExpression left Nothing = left
parseBinaryExpression left (Just (op, right)) = BinaryExpression op left right

parseJassModule :: [Trio [TypeDef] [GlobalVar] [NativeDecl]] -> [Function] -> JassModule
parseJassModule trios functions = let (tdefs, gvars, natives) = foldl accTrios ([], [], []) trios in JassModule tdefs gvars natives functions
    where 
        accTrios (tdefs, gvars, natives) (TrioFirst tdefs') = (tdefs ++ tdefs', gvars, natives)
        accTrios (tdefs, gvars, natives) (TrioSecond gvars') = (tdefs, gvars++gvars', natives)
        accTrios (tdefs, gvars, natives) (TrioThird natives') = (tdefs, gvars, natives++natives')

-- | Parsing jass source
parseJass :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError JassModule -- ^ Error or AST
parseJass = parseString jassModule

-- | Parsing jass source from file
parseJassFile :: FilePath -- ^ File path with jass source 
    -> IO (Either ParseError JassModule) -- ^ Error or AST
parseJassFile = parseFile jassModule        