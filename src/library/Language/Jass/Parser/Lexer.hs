module Language.Jass.Parser.Lexer(
    lexer
  , integer
  , real
  , parens
  , commaSep
  , identifier
  , reserved
  , reservedOp
  , symbol
  , stringLit
  , whiteSpace
  ) where
    
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok
import Text.Parsec ((<|>), (<?>), many1, option)
import Text.Parsec.Char
import Data.Digits
import Data.Char
import GHC.Float
import Control.Monad
import Text.Parsec.Prim (try)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "*", "-", "/", "mod", "==", ">=", "<=", "!=", ">", "<", "and", "or", "not"]
    names = ["type", "extends", "null", "true", "false", "function", "constant"
       , "mod", "and", "or", "not", "native", "returns", "take", "globals", "endglobals"
       , "nothing", "native", "endfunction", "local", "array"
       , "set", "call", "if", "then", "endif", "elseif", "else"
       , "loop", "endloop", "exitwhen", "return", "debug"]
    --lexeme = Tok.lexeme emptyDef
    style = emptyDef {
      Tok.commentLine = "//",
      Tok.commentStart = "/*",
      Tok.commentEnd = "*/",
      Tok.reservedOpNames = ops,
      Tok.reservedNames = names
    }

integer :: Parser Int
integer = try octal <|> try (fmap fromInteger (Tok.integer lexer)) <|> rawCode

rawCode :: Parser Int
rawCode = Tok.lexeme lexer raw <?> "rawcode"
  where 
    raw = do
      _ <- char '\''
      s <- replicateM 4 anyChar
      _ <- char '\''
      return $ unDigits 256 $ map ord s

octal :: Parser Int
octal = Tok.lexeme lexer octal' <?> "octal"
  where 
    octal' = do
      _ <- char '0'
      fromInteger `fmap` number 8 octDigit

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit
        = do{ digs <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digs
            ; seq n (return n)
            }

sign :: Num a => Parser (a -> a)
sign = (char '-' >> return negate) <|> (char '+' >> return id)
    
real :: Parser Float
real = Tok.lexeme lexer real' <?> "real" 
  where 
    real' = fmap double2Float $ do
      s <- option id sign
      n <- option 0 $ Tok.decimal lexer
      _ <- char '.'
      frac <- option 0 fracPart
      exp' <- expPart
      return $ s (exp' (fromIntegral n + frac))
    expPart :: Parser (Double -> Double)
    expPart = option (*1.0) $ do
      _ <- oneOf "eE"
      es <- option id sign
      emantis <- Tok.decimal lexer
      return (* (10.0 ^^ es emantis))
    fracPart :: Parser Double
    fracPart = do
      ds <- many1 digit <?> "fraction"
      return (foldr op 0.0 ds)
    op d f = (f + fromIntegral (digitToInt d))/10.0
    
stringLit :: Parser String
stringLit = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser String
reservedOp s = Tok.reservedOp lexer s >> return s

symbol :: String -> Parser String
symbol = Tok.symbol lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer