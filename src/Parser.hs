module Parser (readExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Datatype (LispVal(..))

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> parseString) "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value"

parseString :: Parser LispVal
-- parseString = do char '"'
--                  x <- many (noneOf "\"")
--                  char '"'
--                  return $ String x
-- parseString = char '"' >> many (noneOf "\"") >>= (\x -> char '"' >> (return $ String x))
