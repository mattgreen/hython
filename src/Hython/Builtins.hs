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
print' objs = do
    strs <- mapM asStr objs
    liftIO $ putStrLn $ unwords strs
  where
    asStr (String s)    = return s
    asStr v@_           = toStr v


