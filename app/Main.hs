module Main where
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
  putStrLn "Please input your name"
  line <- getLine
  putStrLn $ "Hello, " ++ line

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
