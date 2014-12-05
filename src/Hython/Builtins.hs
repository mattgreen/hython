module Hython.Builtins
where

import Prelude hiding (print)

import Data.Complex
import Data.Fixed
import Data.IORef
import Data.List
import Text.Printf

import qualified Hython.AttributeDict as AttributeDict
import Hython.Classes
import Language.Python.Core

builtins :: IO [(String, Object)]
builtins = do
    object <- newClass "object" []
    baseException <- newClass "BaseException" [object]
    exception <- newClass "Exception" [baseException]
    stopIteration <- newClass "StopIteration" [exception]
    arithmeticError <- newClass "ArithmeticError" [exception]
    assertionError <- newClass "AssertionError" [exception]
    attributeError <- newClass "AttributeError" [exception]
    bufferError <- newClass "BufferError" [exception]
    eofError <- newClass "EOFError" [exception]
    importError <- newClass "ImportError" [exception]
    lookupError <- newClass "LookupError" [exception]
    memoryError <- newClass "MemoryError" [exception]
    nameError <- newClass "NameError" [exception]
    osError <- newClass "OSError" [exception]
    referenceError <- newClass "ReferenceError" [exception]
    runtimeError <- newClass "RuntimeError" [exception]
    syntaxError <- newClass "SyntaxError" [exception]
    typeError <- newClass "TypeError" [exception]
    valueError <- newClass "ValueError" [exception]
    warning <- newClass "Warning" [exception]

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

    let builtinFns = map defBuiltin builtinFunctions

    return $ builtinClasses ++ builtinFns

  where
    defBuiltin (name, _) = (name, BuiltinFn name)

builtinFunctions :: [(String, Objects -> IO Object)]
builtinFunctions = [("bool", bool),
                    ("len", len),
                    ("print", print),
                    ("pow", pow),
                    ("slice", slice),
                    ("str", str')]

bool :: Objects -> IO Object
bool([])    = return $ Bool False
bool([x])   = return $ Bool (isTrue x)
bool _      = fail "bool() takes at most 1 argument"

len :: Objects -> IO Object
len([x])    = case x of
                (String value)  -> return $ Int (fromIntegral (length value))
                (List ref)    -> do
                    values <- readIORef ref
                    return $ Int (fromIntegral (length values))
                (Tuple values)  -> return $ Int (fromIntegral (length values))
                _               -> fail "object has no len()"
len _       = fail "len() takes exactly one argument"

pow :: Objects -> IO Object
pow [Int l, Int r]
    | r < 0     = return $ Float (fromIntegral l ^^ r)
    | otherwise = return $ Int $ l ^ r

pow [Int l, Int r, Int m] = do
    result <- pow [Int l, Int r]
    case result of
        Float v     -> return $ Float (v `mod'` fromIntegral m)
        Int v       -> return $ Int (v `mod` m)
        _           -> fail "pow() should produce an Int or Float!"

pow [Float l, Float r] = return $ Float (l ** r)
pow [Float l, Float r, Float m] = do
    (Float result) <- pow [Float l, Float r]
    return $ Float (result `mod'` m)

pow _ = fail "pow() takes at least two arguments"

print :: Objects -> IO Object
print args = do
    stringArgs <- mapM str args
    putStrLn $ unwords stringArgs
    return None

slice :: Objects -> IO Object
slice [end]                 = return $ Slice None end None
slice [start, end, stride]  = return $ Slice start end stride
slice _                     = fail "blah"

str :: Object -> IO String
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
str (Module modInfo)            = return $ printf "<module '%s'>" (moduleName modInfo)
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

str' :: Objects -> IO Object
str' v = do
    s <- str (head v)
    return $ String s

getAttr :: String -> Object -> IO (Maybe Object)
getAttr attr (Object _ ref) = AttributeDict.lookup attr ref
getAttr attr (Class _ _ ref) = AttributeDict.lookup attr ref
getAttr _ _ = fail "Only classes and objects have attrs!"

setAttr :: String -> Object -> Object -> IO ()
setAttr attr value (Object _ ref)   = AttributeDict.update attr value ref
setAttr attr value (Class _ _ ref)  = AttributeDict.update attr value ref
setAttr _ _ _                       = fail "Only objects have attrs!"


