module Builtins (builtinFunctions, BuiltInFunction)
where

import Prelude hiding (print)

import Language

type BuiltInFunction = Values -> IO Value

builtinFunctions :: [(String, BuiltInFunction)]
builtinFunctions = [("print", print), ("str", str)]

print :: Values -> IO Value
print args = do
    let s = show $ head args
    putStrLn s
    return None

str :: Values -> IO Value
str v = do
    let s = show (head v)
    return $ String s

