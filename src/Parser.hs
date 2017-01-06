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

parseNumber :: Parser LispVal
parseNumber = do
  s <- many1 digit
       <|> try (string "#b") <|> try (string "#o")
       <|> try (string "#d") <|> try (string "#x")
  if isDigit . head $ s
    then return . Number . read $ s
    else case s of
           "#b" -> many1 (oneOf "01")               >>= toNumber . readBin
           "#o" -> many1 (oneOf "01234567")         >>= toNumber . readOct
           "#d" -> many1 (oneOf "0123456789")       >>= toNumber . readDec
           "#x" -> many1 (oneOf "0123456789abcdef") >>= toNumber . readHex
  where toNumber = return . Number . fst . head

parseFloat :: Parser LispVal
parseFloat = do
  b <- many1 digit
  _ <- char '.'
  x <- many1 digit
  return . Float . read $ b ++ "." ++ x

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
parseExpr = try parseFloat
            <|> parseNumber
            <|> parseChar
            <|> parseAtom
            <|> parseString

printString :: Either ParseError LispVal -> IO ()
printString (Right (String s)) = putStrLn s
printString _ = putStrLn "Print error"
