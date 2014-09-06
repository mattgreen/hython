module Builtins
where

import Prelude hiding (print)

import Data.Complex
import Data.Fixed
import Data.IORef
import Data.List
import Text.Printf

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
                (List ref)    -> do
                    values <- readIORef ref
                    return $ Int (fromIntegral (length values))
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
    stringArgs <- mapM str args
    putStrLn $ unwords stringArgs
    return None

slice :: Values -> IO Value
slice (end:[])              = return $ Slice None end None
slice (start:end:stride:[]) = return $ Slice start end stride
slice _                     = fail "blah"

str :: Value -> IO String
str None                        = return "None"
str (Bool v)                    = return $ show v
str (String v)                  = return v
str (Int v)                     = return $ show v
str (Float v)                   = return $ show v
str (Imaginary v)
    | realPart v == 0           = return $ show (imagPart v) ++ "j"
    | otherwise                 = return $ show v
str (Function name _ _)         = return $ printf "<%s>" name
str (BuiltinFn name)            = return $ printf "<built-in function %s>" name
str (Class name _)              = return $ printf "<class '__main__.%s'>" name
str (Object (Class name _) _)   = return $ printf "<%s object>" name
str (Object _ _)                = return "<invalid object>"
str (Slice start end stride) =
    return $ printf "slice(%s, %s, %s)" (show start) (show end) (show stride)
str (Tuple values) = do
    stringValues <- mapM str values
    return $ printf "(%s%s)" (intercalate ", " stringValues) trailer

    where
        trailer = case values of
                      [_]   -> ","
                      _     -> ""
str (List ref) = do
    values <- readIORef ref
    stringValues <- mapM str values
    return $ printf "[%s]" (intercalate ", " stringValues)

str' :: Values -> IO Value
str' v = do
    s <- str (head v)
    return $ String s

