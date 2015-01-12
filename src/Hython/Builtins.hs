module Hython.Builtins
where

import Prelude hiding (print)

import Control.Arrow
import Data.Complex
import Data.Fixed
import Data.IORef
import Data.List
import Text.Printf

import qualified Hython.AttributeDict as AttributeDict
import Hython.Class
import Hython.Object

builtins :: IO [(String, Object)]
builtins = do
    emptyDict <- AttributeDict.empty
    let builtinsModule = Module {
        moduleName = "__builtins__",
        modulePath = "<builtin>",
        moduleDict = emptyDict
    }

    object <- newClass "object" [] builtinsModule
    let builtinClasses = [("object", object)]
    let builtinFns = map defBuiltin builtinFunctions

    return $ map (Control.Arrow.second ClassObj) builtinClasses ++ builtinFns

  where
    defBuiltin (name, _) = (name, BuiltinFn name)

    newClass name bases owner = do
        emptyDict <- AttributeDict.empty

        return Class {
            className = name,
            classBases = bases,
            classDict = emptyDict,
            classModule = owner
        }

builtinFunctions :: [(String, Objects -> IO Object)]
builtinFunctions = [("bool", bool),
                    ("len", len),
                    ("print", print),
                    ("pow", pow'),
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

pow' :: Objects -> IO Object
pow' args = return $ pow args

pow :: Objects -> Object
pow [Int l, Int r]
    | r < 0     = Float (fromIntegral l ^^ r)
    | otherwise = Int $ l ^ r

pow [Int l, Int r, Int m] = case pow [Int l, Int r] of
    Float v     -> Float (v `mod'` fromIntegral m)
    Int v       -> Int (v `mod` m)
    _           -> error "pow() should produce an Int or Float!"

pow [Float l, Float r] = Float (l ** r)
pow [Float l, Float r, Float m] = do
    let (Float result) = pow [Float l, Float r]
    Float (result `mod'` m)

pow _ = error "pow() takes at least two arguments"

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
str (Lambda {})                 = return $ printf "<function <lambda>"
str (Function name _ _ _)       = return $ printf "<%s>" name
str (BuiltinFn name)            = return $ printf "<built-in function %s>" name
str (ModuleObj info)            = return $ printf "<module '%s'>" (moduleName info)
str (ClassObj cls)              = return $ printf "<class '__main__.%s'>" (className cls)
str (Object cls _)              = return $ printf "<%s object>" (className cls)
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
getAttr attr (Object cls dict) = do
    obj <- AttributeDict.lookup attr dict
    case obj of
        Just o  -> return $ Just o
        Nothing -> lookupAttr attr cls

getAttr attr (ClassObj cls)     = lookupAttr attr cls
getAttr attr (ModuleObj m)      = AttributeDict.lookup attr (moduleDict m)
getAttr _ _ = fail "Only classes and objects have attrs!"

setAttr :: String -> Object -> Object -> IO ()
setAttr attr value (Object _ ref)   = AttributeDict.update attr value ref
setAttr attr value (ClassObj cls)   = AttributeDict.update attr value (classDict cls)
setAttr attr value (ModuleObj m)    = AttributeDict.update attr value (moduleDict m)
setAttr _ _ _                       = fail "Only objects have attrs!"


