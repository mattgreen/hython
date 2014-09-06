module Builtins (builtinFunctions, pow, BuiltInFunction)
where

import Prelude hiding (print)
import Data.Fixed

import Language

type BuiltInFunction = Values -> IO Value

builtinFunctions :: [(String, BuiltInFunction)]
builtinFunctions = [("bool", bool), ("len", len), ("print", print), ("pow", pow), ("slice", slice), ("str", str')]

bool :: Values -> IO Value
bool([])    = return $ Bool False
bool([x])   = return $ Bool (isTrue x)
bool _      = fail "bool() takes at most 1 argument"

len :: Values -> IO Value
len([x])    = case x of
                (String value)  -> return $ Int (fromIntegral (length value))
                (Tuple values)  -> return $ Int (fromIntegral (length values))
                _               -> fail "object has no len()"
len _       = fail "len() takes exactly one argument"

pow :: Values -> IO Value
pow (Int l:Int r:[])
    | r < 0     = return $ Float (fromIntegral l ^^ r)
    | otherwise = return $ Int $ l ^ r

pow (Int l:Int r:Int m:[]) = do
    result <- pow [Int l, Int r]
    case result of
        Float v    -> return $ Float (v `mod'` fromIntegral m)
        Int v      -> return $ Int (v `mod` m)

pow (Float l:Float r:[]) = return $ Float (l ** r)
pow (Float l:Float r:Float m:[]) = do
    (Float result) <- pow [Float l, Float r]
    return $ Float (result `mod'` m)

pow _ = fail "pow() takes at least two arguments"

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

