module Main where
import System.Environment
import Lib


main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0 ++ " " ++ (show . doubleX $ 3))
