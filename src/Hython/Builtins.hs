module Hython.Builtins where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef)
import Data.List (intercalate)
import Data.IntMap (elems)

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

toStr :: MonadIO m => Object -> m String
toStr (None) = return "None"
toStr (Bool b) = return $ if b then "True" else "False"
toStr (Bytes _b) = return "b'??'"
toStr (Float f) = return $ show f
toStr (Imaginary i) = return $ show i
toStr (Int i) = return $ show i
toStr (String s) = return $ "'" ++ s ++ "'"
toStr (List ref) = do
    l <- liftIO $ readIORef ref
    strItems <- mapM toStr l
    return $ "[" ++ intercalate ", " strItems ++ "]"
toStr (Tuple objs) = do
    strItems <- mapM toStr objs
    case strItems of
        [str]   -> return $ "(" ++ str ++ ",)"
        _       -> return $ "(" ++ intercalate ", " strItems ++ ")"
toStr (Set ref) = do
    items <- liftIO $ readIORef ref
    strItems <- mapM toStr $ elems items
    return $ "{" ++ intercalate ", " strItems ++ "}"
toStr (Dict ref) = do
    items <- liftIO $ readIORef ref
    strItems <- forM (elems items) $ \(k, v) -> do
        key     <- toStr k
        value   <- toStr v
        return $ key ++ ": " ++ value
    return $ "{" ++ intercalate ", " strItems ++ "}"
toStr (BuiltinFn name)  = return $ "<built-in function " ++ name ++ ">"
