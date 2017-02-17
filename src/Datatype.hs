module Datatype where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Complex (Double,Double)
             | Rational (Integer,Integer)
             | String String
             | Bool Bool
             | Character Char
