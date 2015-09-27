module Hython.Builtins where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef, writeIORef)

import qualified Hython.AttributeDict as AttributeDict
import Hython.Types

builtinFunctions :: [String]
builtinFunctions = ["print"]

callBuiltin :: (MonadInterpreter m) => String -> [Object] -> m Object
callBuiltin name args = do
    case (name, args) of
        ("print", _)    -> print' args
        (_, _)          -> raise "SystemError" ("builtin '" ++ name ++ "' not implemented!")
    return None

getAttr :: (MonadInterpreter m) => String -> Object -> m (Maybe Object)
{-getAttr attr (Class cls) = -}
getAttr attr target = case getObjAttrs target of
        Just ref -> do
            dict <- liftIO $ readIORef ref
            mobj <- AttributeDict.lookup attr dict
            case mobj of
                Just obj -> return $ Just $ case (target, obj) of
                    (Class info, Function name params body) ->
                        Method name (ClassBinding (className info) target) params body
                    (Object info, Function name params body) ->
                        Method name (InstanceBinding (className $ objectClass info) target) params body
                    _ -> obj
                Nothing -> return Nothing

        Nothing -> do
            raise "TypeError" "object does not have attributes"
            return Nothing

print' :: MonadIO m => [Object] -> m ()
print' [] = liftIO $ putStrLn ""
print' objs = do
    strs <- mapM asStr objs
    liftIO $ putStrLn $ unwords strs
  where
    asStr (String s)    = return s
    asStr v@_           = toStr v

setAttr :: (MonadInterpreter m) => String -> Object -> Object -> m ()
setAttr attr obj target = case getObjAttrs target of
        Just ref -> do
            dict    <- liftIO $ readIORef ref
            dict'   <- AttributeDict.set attr obj dict
            liftIO $ writeIORef ref dict'
        Nothing -> raise "TypeError" "object does not have attributes"
