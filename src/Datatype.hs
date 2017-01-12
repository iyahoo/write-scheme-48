module Datatype where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Complex (Double,Double)
             | String String
             | Bool Bool
             | Character Char
             deriving (Show)
