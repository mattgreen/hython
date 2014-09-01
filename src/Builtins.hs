module Builtins (builtinFunctions, BuiltInFunction)
where

import Prelude hiding (print)

import Language

type BuiltInFunction = Values -> IO Value

builtinFunctions :: [(String, BuiltInFunction)]
builtinFunctions = [("bool", bool), ("print", print), ("slice", slice), ("str", str')]

bool :: Values -> IO Value
bool([])    = return $ Bool False
bool([x])   = return $ Bool (isTrue x)
bool _      = fail "bool() takes at most 1 argument"

print :: Values -> IO Value
print args = do
    let s = unwords $ map str args
    putStrLn s
    return None

slice :: Values -> IO Value
slice (end:[])              = return $ Slice None end None
slice (start:end:stride:[]) = return $ Slice start end stride
slice _                     = fail "blah"

str' :: Values -> IO Value
str' v = do
    let s = str (head v)
    return $ String s

