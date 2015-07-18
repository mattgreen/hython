module Hython.Builtins where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Hython.Object

builtinFunctions :: [String]
builtinFunctions = ["print"]

callBuiltin :: (MonadInterpreter m, MonadIO m) => String -> [Object] -> m Object
callBuiltin name args = do
    case (name, args) of
        ("print", _)    -> print' args
        (_, _)          -> raise "SystemError" ("builtin '" ++ name ++ "' not implemented!")
    return None

print' :: MonadIO m => [Object] -> m ()
print' [] = liftIO $ putStrLn ""
print' objs = liftIO $ putStrLn $ unwords $ map asStr objs
  where
    asStr (String s)    = s
    asStr v@_           = toStr v

toStr :: Object -> String
toStr (None) = "None"
toStr (Bool b) = if b then "True" else "False"
toStr (Bytes _b) = "b'??'"
toStr (Float f) = show f
toStr (Imaginary i) = show i
toStr (Int i) = show i
toStr (String s) = "'" ++ s ++ "'"
toStr (BuiltinFn name)  = "<built-in function " ++ name ++ ">"
