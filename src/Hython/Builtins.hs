module Hython.Builtins where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Hython.AttributeDict as AttributeDict
import qualified Hython.Class as Class
import qualified Hython.Object as Object
import Hython.Types

builtinFunctions :: [String]
builtinFunctions = ["isinstance", "print"]

callBuiltin :: (MonadInterpreter m) => String -> [Object] -> m Object
callBuiltin name args = case (name, args) of
    ("print", _) -> ignore $ print' args
    ("isinstance", [obj, Class info]) -> newBool $ isInstance obj info
    ("isinstance", _) -> ignore $ raise "SystemError" "isinstance() arg 2 must be a class"
    _ -> ignore $ raise "SystemError" ("builtin '" ++ name ++ "' not implemented!")
  where
    ignore action = action >> return None

getAttr :: (MonadInterpreter m) => Text -> Object -> m (Maybe Object)
getAttr attr target = runMaybeT $ do
    obj <- MaybeT $ case target of
        (Class info)    -> Class.lookup attr info
        (Object info)   -> Object.lookup attr info
        _               -> do
            raise "TypeError" "object does not have attributes"
            return Nothing

    case (target, obj) of
        (Class info, Function name params body) ->
            return $ Method name (ClassBinding (className info) target) params body

        (Object info, Function name params body) ->
            return $ Method name (InstanceBinding (className $ objectClass info) target) params body
        _               -> return obj

isInstance :: Object -> ClassInfo -> Bool
isInstance (Object info) cls = objectClass info == cls
isInstance (Class info) cls = info == cls
isInstance _ _ = False

print' :: MonadIO m => [Object] -> m ()
print' [] = liftIO $ putStrLn ""
print' objs = do
    strs <- mapM asStr objs
    liftIO $ putStrLn $ unwords strs
  where
    asStr (String s)    = return . T.unpack $ s
    asStr v@_           = toStr v

setAttr :: (MonadInterpreter m) => Text -> Object -> Object -> m ()
setAttr attr obj target = case getObjAttrs target of
        Just ref -> do
            dict    <- liftIO $ readIORef ref
            dict'   <- AttributeDict.set attr obj dict
            liftIO $ writeIORef ref dict'
        Nothing -> raise "TypeError" "object does not have attributes"
