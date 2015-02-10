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
    
import Data.Maybe
import Language.Jass.Parser.AST as AST
import Language.Jass.Parser.Lexer
import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (parseFromFile)
import Control.Applicative ((<$>), (<*>), pure)

data Trio a b c = TrioFirst a | TrioSecond b | TrioThird c

type JassParser = Parsec String ()

getPosition' :: JassParser SrcPos
getPosition' = do
    pos <- getPosition
    return $ SrcPos (sourceName pos) (sourceLine pos) (sourceColumn pos)
    
jassModule :: JassParser JassModule
jassModule = do
    pos <- getPosition'
    imports <- many parseImport
    trios <- many $ 
        try (fmap TrioFirst (many1 typeDef)) <|>
        try (fmap TrioSecond globalVars) <|>
        try (fmap TrioThird (many1 nativeDecl))
    funcs <- many function
    return $ parseJassModule pos imports trios funcs 

parseImport :: JassParser Import
parseImport = do
  pos <- getPosition'
  reserved "import"
  qualParts <- identifier `sepBy1` char '.'
  let moduleName = foldr1 (\w s -> w ++ '.':s) qualParts
  return $ Import pos moduleName
  
typeDef :: JassParser TypeDef
typeDef = do
  pos <- getPosition'
  reserved "type"
  name <- identifier
  reserved "extends"
  tp <- jassType
  return $ TypeDef pos name tp

jassType :: JassParser JassType
jassType = 
  try (reserved "integer" >> return JInteger) <|>
  try (reserved "real" >> return JReal) <|>
  try (reserved "boolean" >> return JBoolean) <|>
  try (reserved "string" >> return JString) <|>
  try (reserved "handle" >> return JHandle) <|>
  try (reserved "code" >> return JCode) <|>
  (JUserDefined <$> identifier) <?> "unknown jass type"

isConstant :: JassParser Bool
isConstant = fmap isJust $ optionMaybe $ reserved "constant"

globalVars :: JassParser [GlobalVar]
globalVars = do
  reserved "globals"
  vars <- many globalVar
  reserved "endglobals"
  return vars

globalVar :: JassParser GlobalVar
globalVar = do
  pos <- getPosition'
  cnst <- isConstant
  tp <- jassType
  try (podVarDecl pos cnst tp) <|> arrayVarDecl pos cnst tp
  where
    podVarDecl pos cnst tp = do
      name <- identifier
      initExpr <- optionMaybe (reservedOp "=" >> expression)
      return $ GlobalVar pos cnst False tp name initExpr
    arrayVarDecl pos cnst tp = do
      reserved "array"
      name <- identifier
      return $ GlobalVar pos cnst True tp name Nothing

nativeDecl :: JassParser NativeDecl
nativeDecl = do
  pos <- getPosition'
  cnst <- isConstant
  reserved "native"
  fdecl <- functionDecl
  return $ NativeDecl pos cnst fdecl

functionDecl :: JassParser FunctionDecl
functionDecl = do
  pos <- getPosition'
  name <- identifier
  reserved "takes"
  params <- try (fmap (const []) (reserved "nothing")) <|> paramList
  reserved "returns"
  retType <- try (fmap (const Nothing) (reserved "nothing")) <|> fmap Just jassType
  return $ FunctionDecl pos name params retType
  where
    paramList = commaSep $ Parameter <$> getPosition' <*> jassType <*> identifier

function :: JassParser Function
function = do
  pos <- getPosition'
  cnst <- isConstant
  reserved "function"
  fdec <- functionDecl
  locals <- many localVar
  stmts <- many statement
  reserved "endfunction"
  return $ Function pos cnst fdec locals stmts

localVar :: JassParser LocalVar
localVar = reserved "local" >> (try podLocalVar <|> arrayLocalVar)
  where
    podLocalVar = LocalVar <$> getPosition' <*> pure False <*> jassType <*> identifier <*> optionMaybe (reservedOp "=" >> expression)
    arrayLocalVar = LocalVar <$> getPosition' <*> pure True <*> jassType <*> (reserved "array" >> identifier) <*> pure Nothing 

binOp :: String -> BinaryOperator
binOp "or" = Or
binOp "and" = And
binOp "==" = Equal
binOp "!=" = NotEqual
binOp ">" = Greater
binOp "<" = Less
binOp ">=" = GreaterEqual
binOp "<=" = LessEqual
binOp "+" = Summ
binOp "-" = Substract
binOp "*" = Multiply
binOp "/" = Divide
binOp "mod" = Reminder 
binOp s = error $ "unknown binary operator " ++ s

parseBinOp :: String -> JassParser BinaryOperator
parseBinOp s = reservedOp s >> return (binOp s)

unaryOp :: String -> UnaryOperator
unaryOp "+" = Plus
unaryOp "-" = Negation
unaryOp "not" = Not
unaryOp s = error $ "unknown unary operator " ++ s

parseUnaryOp :: String -> JassParser UnaryOperator
parseUnaryOp s = reservedOp s >> return (unaryOp s)

expression :: JassParser Expression
expression = orExpression
  where
    orExpression = expressionLevel andExpression ["or"]
    andExpression = expressionLevel equalityExpression ["and"]
    equalityExpression = expressionLevel relationalExpression ["==", "!="]
    relationalExpression = expressionLevel additiveExpression [">=", "<=", ">", "<"]
    additiveExpression = expressionLevel multiplicativeExpression ["+", "-"]
    multiplicativeExpression = expressionLevel unaryExpression ["*", "/", "mod"]
    
    expressionLevel nextLvl ops = do
      pos <- getPosition'
      left <- nextLvl
      right <- optionMaybe $ (,) <$> choice (parseBinOp <$> ops) <*> expressionLevel nextLvl ops
      return $ parseBinaryExpression pos left right
      
    unaryExpression = postfixExpression <|> 
      UnaryExpression <$> getPosition' <*> choice (parseUnaryOp <$> ["+", "-", "not"]) <*> unaryExpression
    postfixExpression = choice [try arrayRef, try funcCall, try funcRef, try primaryExpression]
    arrayRef = ArrayReference <$> getPosition' <*> identifier <*> between (symbol "[") (symbol "]") expression
    funcCall = FunctionCall <$> getPosition' <*> identifier <*> between (symbol "(") (symbol ")") (commaSep expression)
    funcRef = FunctionReference <$> getPosition' <*> (reserved "function" >> identifier)
    varRef = VariableReference <$> getPosition' <*> identifier
    primaryExpression = choice [try literal, try varRef, parens expression]
    
    literal = choice [try realLiteral, try intLiteral, try boolLiteral, try nullLiteral, stringLiteral]
    nullLiteral = reserved "null" >> NullLiteral <$> getPosition'
    boolLiteral = BoolLiteral <$> getPosition' <*> ((reserved "true" >> return True) <|> (reserved "false" >> return False))
    realLiteral = RealLiteral <$> getPosition' <*> real
    intLiteral = IntegerLiteral <$> getPosition' <*> integer
    stringLiteral = StringLiteral <$> getPosition' <*> stringLit
    
statement :: JassParser Statement
statement = choice $ try <$> [setStatement, callStatement, ifThenElseStatement, loopStatement, 
  exitWhenStatement, returnStatement, debugStatement]
  where
    setStatement = do
      pos <- getPosition'
      reserved "set"
      name <- identifier
      try (setPodStatement pos name) <|> setArrayStatement pos name
    setPodStatement pos name = do
      reserved "="
      expr <- expression
      return $ SetStatement pos False name expr
    setArrayStatement pos name = do
      indExpr <- between (symbol "[") (symbol "]") expression
      reserved "="
      expr <- expression
      return $ SetArrayStatement pos False name indExpr expr
    callStatement = do
      pos <- getPosition'
      reserved "call"
      name <- identifier
      args <- between (symbol "(") (symbol ")") $ commaSep expression
      return $ CallStatement pos False name args
    ifThenElseStatement = do
      pos <- getPosition'
      reserved "if"
      cond <- expression
      reserved "then"
      thenStmts <- many statement
      elses <- option [] elseClauses
      reserved "endif"
      return $ IfThenElseStatement pos False cond thenStmts elses
    elseClauses = choice $ fmap try [(
      do
        reserved "elseif"
        cond <- expression
        reserved "then"
        stmts <- many statement
        nextElse <- option [] elseClauses
        return $ (Just cond, stmts) : nextElse) <?> "elseif clause"
      , (
      do
        reserved "else"
        stmts <- many statement
        return [(Nothing, stmts)]
      ) <?> "else clause"]
    
    loopStatement = do
      pos <- getPosition'
      reserved "loop"
      stmts <- many statement
      reserved "endloop"
      return $ LoopStatement pos False stmts
    exitWhenStatement = do
      pos <- getPosition'
      reserved "exitwhen"
      ExitWhenStatement pos <$> expression
    returnStatement = do
      pos <- getPosition'
      reserved "return"
      ReturnStatement pos <$> optionMaybe expression
    debugStatement = do
      reserved "debug"
      stmt <- choice $ try <$> [setStatement, callStatement, ifThenElseStatement, loopStatement]
      return $ setDebugStatement True stmt
        
parseBinaryExpression :: SrcPos -> Expression -> Maybe (BinaryOperator, Expression) -> Expression
parseBinaryExpression _ left Nothing = left
parseBinaryExpression pos left (Just (op, right)) = BinaryExpression pos op left right

parseJassModule :: SrcPos -> [Import] -> [Trio [TypeDef] [GlobalVar] [NativeDecl]] -> [Function] -> JassModule
parseJassModule pos imports trios functions = let (tdefs, gvars, natives) = foldl accTrios ([], [], []) trios 
  in JassModule pos imports tdefs gvars natives functions
  where 
      accTrios (tdefs, gvars, natives) (TrioFirst tdefs') = (tdefs ++ tdefs', gvars, natives)
      accTrios (tdefs, gvars, natives) (TrioSecond gvars') = (tdefs, gvars++gvars', natives)
      accTrios (tdefs, gvars, natives) (TrioThird natives') = (tdefs, gvars, natives++natives')

-- | Parsing whole file/source
contents :: JassParser a -> JassParser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r
  
-- | Parsing jass source
parseJass :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError JassModule -- ^ Error or AST
parseJass = parse $ contents jassModule

-- | Parsing jass grammar subset 
parseJassFunction :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError Function -- ^ Error or AST
parseJassFunction = parse $ contents function

-- | Parsing jass grammar subset 
parseJassLocalVar :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError LocalVar -- ^ Error or AST
parseJassLocalVar = parse $ contents localVar

-- | Parsing jass grammar subset 
parseJassGlobalVar :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError GlobalVar -- ^ Error or AST
parseJassGlobalVar = parse $ contents globalVar

-- | Parsing jass grammar subset (debug function)
parseJassTwoGlobalVars :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError (GlobalVar, GlobalVar) -- ^ Error or AST
parseJassTwoGlobalVars = parse $ contents $ do
  g1 <- globalVar
  g2 <- globalVar
  return (g1, g2)

-- | Parsing jass grammar subset 
parseJassGlobalVars :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError [GlobalVar] -- ^ Error or AST
parseJassGlobalVars = parse $ contents globalVars

-- | Parsing jass grammar subset 
parseJassTypeDef :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError TypeDef -- ^ Error or AST
parseJassTypeDef = parse $ contents typeDef

-- | Parsing jass grammar subset 
parseJassNative :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError NativeDecl -- ^ Error or AST
parseJassNative = parse $ contents nativeDecl

-- | Parsing jass grammar subset 
parseJassExpression :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError Expression -- ^ Error or AST
parseJassExpression = parse expression

-- | Parsing jass grammar subset 
parseJassStatement :: String -- ^ Name of input (for displaying errors messages)
    -> String -- ^ String with jass source 
    -> Either ParseError Statement -- ^ Error or AST
parseJassStatement = parse $ contents statement

-- | Parsing jass source from file
parseJassFile :: FilePath -- ^ File path with jass source 
    -> IO (Either ParseError JassModule) -- ^ Error or AST
parseJassFile = parseFromFile $ contents jassModule        