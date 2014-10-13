module Builtins
where

import Prelude hiding (print)

import Data.Complex
import Data.Fixed
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.List
import Text.Printf

import Language

type BuiltInFunction = Values -> IO Value

evalBuiltinFn :: String -> Values -> IO Value
evalBuiltinFn "bool"    = bool
evalBuiltinFn "len"     = len
evalBuiltinFn "print"   = print
evalBuiltinFn "pow"     = pow
evalBuiltinFn "slice"   = slice
evalBuiltinFn "str"     = str'
evalBuiltinFn name      = fail $ "no built-in with name " ++ name

builtins :: IO [(String, Value)]
builtins = do
    object <- defClass "object" []
    baseException <- defClass "BaseException" [object]
    exception <- defClass "Exception" [baseException]
    stopIteration <- defClass "StopIteration" [exception]
    arithmeticError <- defClass "ArithmeticError" [exception]
    assertionError <- defClass "AssertionError" [exception]
    attributeError <- defClass "AttributeError" [exception]
    bufferError <- defClass "BufferError" [exception]
    eofError <- defClass "EOFError" [exception]
    importError <- defClass "ImportError" [exception]
    lookupError <- defClass "LookupError" [exception]
    memoryError <- defClass "MemoryError" [exception]
    nameError <- defClass "NameError" [exception]
    osError <- defClass "OSError" [exception]
    referenceError <- defClass "ReferenceError" [exception]
    runtimeError <- defClass "RuntimeError" [exception]
    syntaxError <- defClass "SyntaxError" [exception]
    typeError <- defClass "TypeError" [exception]
    valueError <- defClass "ValueError" [exception]
    warning <- defClass "Warning" [exception]

    let builtinClasses = [("object", object),
                          ("BaseException", baseException),
                          ("Exception", exception),
                          ("StopIteration", stopIteration),
                          ("ArithmeticError", arithmeticError),
                          ("AssertionError", assertionError),
                          ("AttributeError", attributeError),
                          ("BufferError", bufferError),
                          ("EOFError", eofError),
                          ("ImportError", importError),
                          ("LookupError", lookupError),
                          ("MemoryError", memoryError),
                          ("NameError", nameError),
                          ("OSError", osError),
                          ("ReferenceError", referenceError),
                          ("RuntimeError", runtimeError),
                          ("SyntaxError", syntaxError),
                          ("TypeError", typeError),
                          ("ValueError", valueError),
                          ("Warning", warning)]

    let builtinFns = map defBuiltin ["bool", "len", "print", "pow", "slice", "str"]

    return $ builtinClasses ++ builtinFns

  where
    defBuiltin name = (name, BuiltinFn name)

{-builtinFunctions :: [(String, BuiltInFunction)]-}
{-builtinFunctions = [("bool", bool), ("len", len), ("print", print), ("pow", pow), ("slice", slice), ("str", str')]-}

bool :: Values -> IO Value
bool([])    = return $ Bool False
bool([x])   = return $ Bool (isTrue x)
bool _      = fail "bool() takes at most 1 argument"

classOf :: Value -> Value
classOf (Object c@(Class {}) _) = c
{-classOf _ = fail "classOf requires an object to be passed"-}

isSubClass :: Value -> Value -> Bool
isSubClass (Class derivedName bases _) (Class baseName _ _) = derivedName == baseName || any isBase bases
  where
    isBase (Class base _ _) = baseName == base
{-isSubClass _ _ = fail "isSubClass() arg 1 must be a class"-}

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
pow [Int l, Int r]
    | r < 0     = return $ Float (fromIntegral l ^^ r)
    | otherwise = return $ Int $ l ^ r

pow [Int l, Int r, Int m] = do
    result <- pow [Int l, Int r]
    case result of
        Float v    -> return $ Float (v `mod'` fromIntegral m)
        Int v      -> return $ Int (v `mod` m)

pow [Float l, Float r] = return $ Float (l ** r)
pow [Float l, Float r, Float m] = do
    (Float result) <- pow [Float l, Float r]
    return $ Float (result `mod'` m)

pow _ = fail "pow() takes at least two arguments"

print :: Values -> IO Value
print args = do
    stringArgs <- mapM str args
    putStrLn $ unwords stringArgs
    return None

slice :: Values -> IO Value
slice [end]                 = return $ Slice None end None
slice [start, end, stride]  = return $ Slice start end stride
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
str (Class name _ _)            = return $ printf "<class '__main__.%s'>" name
str (Object (Class name _ _) _) = return $ printf "<%s object>" name
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

defClass :: String -> Values -> IO Value
defClass name bases = do
    dict <- newIORef Map.empty
    return $ Class name bases dict
