module Builtins (builtinFunctions, BuiltInFunction)
where

import Prelude hiding (print)

import Data.Complex
import Data.List
import Text.Printf

import Language

type BuiltInFunction = Values -> IO Value

builtinFunctions :: [(String, BuiltInFunction)]
builtinFunctions = [("print", print), ("str", str)]

print :: Values -> IO Value
print args = do
    let s = toString $ head args
    putStrLn s
    return None

str :: Values -> IO Value
str v = do
    let s = toString (head v)
    return $ String s

toString :: Value -> String
toString (None) = "None"
toString (Bool v) = show v
toString (String v) = v
toString (Int v) = show v
toString (Float v) = show v
toString (Imaginary v)
    | realPart v == 0   = show (imagPart v) ++ "j"
    | otherwise         = show v
toString (Function name _ _) = printf "<%s>" name
toString (BuiltinFn name) = printf "<built-in function %s>" name
toString (Class name _) = printf "<class '__main__.%s'>" name
toString (Object (Class name _) _) = printf "<%s object>" name
toString (Object _ _) = fail "Object must be associated with a class!"
toString (Slice start end stride) =
    printf "slice(%s, %s, %s)" (toString start) (toString end) (toString stride)
toString (Tuple values) =
    printf "(%s%s)" (intercalate ", " stringValues) trailer
  where
    stringValues = map toString values
    trailer = case values of
        [_]   -> ","
        _     -> ""


