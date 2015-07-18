module Hython.Builtins where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Hython.Object

builtinFunctions :: [String]
builtinFunctions = ["print"]

callBuiltin :: MonadIO m => String -> [Object] -> m (Maybe Object)
callBuiltin "print" args = do
    result <- print' args
    return $ Just result
callBuiltin _ _ = return Nothing

print' :: MonadIO m => [Object] -> m Object
print' [] = liftIO $ putStrLn "" >> return None
print' objs = do
    liftIO $ putStrLn $ unwords $ map asStr objs
    return None
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
