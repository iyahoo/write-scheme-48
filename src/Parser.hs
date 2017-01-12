module Parser (readExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Datatype (LispVal(..))
import Control.Monad
import Numeric
import Data.Char

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value"

parseEscape :: Parser Char
parseEscape = do
  char '\\'
  x <- oneOf "\"nrt\\"
  return $ case x of
             'n' -> '\n'
             'r' -> '\r'
             't' -> '\t'
             _   -> x

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ parseEscape <|> noneOf "\""
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

readBin :: (Num a, Eq a) => ReadS a
readBin = readInt 2 (\c -> elem c "01") digitToInt

parseNumber' :: Parser String
parseNumber' = do
  s <- many1 digit
       <|> try (string "#b") <|> try (string "#o")
       <|> try (string "#d") <|> try (string "#x")
  if isDigit . head $ s
    then return s
    else case s of
           "#b" -> many1 (oneOf "01")               >>= toString . readBin
           "#o" -> many1 (oneOf "01234567")         >>= toString . readOct
           "#d" -> many1 (oneOf "0123456789")       >>= toString . readDec
           "#x" -> many1 (oneOf "0123456789abcdef") >>= toString . readHex
  where toString = return . show . fst . head

parseNumber :: Parser LispVal
parseNumber = do
  x <- parseNumber'
  return . Number . read $ x

parseFloat' :: Parser String
parseFloat' = do
  b <- many1 digit
  dot <- char '.'
  x <- many1 digit
  return $ b ++ [dot] ++ x

parseFloat :: Parser LispVal
parseFloat =  parseFloat' >>= return . Float . read 

parseComplex :: Parser LispVal
parseComplex = do
  r <- try parseFloat' <|> try parseNumber'
  s <- char '+' <|> char '-'
  i <- try (many parseFloat') <|> try (many parseNumber')
  _ <- char 'i'
  let sign = if s == '+' then ' ' else '-'
  let im = if i == [] then ["1"] else i
  return . Complex $ (read r, read $ [sign] ++ head im)

parseChar :: Parser LispVal
parseChar = do
  try (string "#\\")
  c <- try (string "newline") <|> try (string "space")
       <|> do { x <- anyChar; return (x:[])}
  return . Character $ case c of
                         "newline" -> '\n'
                         "space"   -> ' '
                         _         -> head c

parseExpr :: Parser LispVal
parseExpr = try parseComplex
            <|> try parseFloat
            <|> parseNumber
            <|> parseChar
            <|> parseAtom
            <|> parseString

printString :: Either ParseError LispVal -> IO ()
printString (Right (String s)) = putStrLn s
printString _ = putStrLn "Print error"
