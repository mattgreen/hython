{-# LANGUAGE OverloadedStrings #-}

module Hython.Builtins where

import Control.Monad.Trans.Maybe
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T

import qualified Hython.AttributeDict as AttributeDict
import qualified Hython.Class as Class
import qualified Hython.Module as Module
import qualified Hython.Object as Object
import Hython.Primitive (callPrimitive)
import Hython.Ref
import Hython.Types

builtinFunctions :: [Text]
builtinFunctions = map T.pack builtins
  where
    builtins = ["__hython_primitive__", "isinstance", "len"]

callBuiltin :: (MonadInterpreter m) => Text -> [Object] -> m Object
callBuiltin name args = case (T.unpack name, args) of
    ("__hython_primitive__", primArg : remaining) ->
        case primArg of
            (String prim)   -> callPrimitive (T.unpack prim) remaining
            _               -> ignore $ raise "SystemError" "arg 1 must be string"

    ("__hython_primitive__", []) ->
        ignore $ raise "SystemError" "__hython_primitive__ requires at least one arg"

    ("len", [obj]) -> newInt . toInteger =<< len obj
    ("len", _) ->
        ignore $ raise "SystemError" "len() takes only one arg"
    ("isinstance", [obj, Class info]) ->
        newBool $ isInstance obj info
    ("isinstance", _) ->
        ignore $ raise "SystemError" "isinstance() arg 2 must be a class"
    _ ->
        ignore $ raise "SystemError" ("builtin '" ++ show name ++ "' not implemented!")
  where
    ignore action = action >> return None

getAttr :: (MonadInterpreter m) => Text -> Object -> m (Maybe Object)
getAttr attr target = runMaybeT $ do
    obj <- MaybeT $ case (attr, target) of
        ("__name__", Class info)    -> return . Just =<< newString (className info)
        (_, Class info)             -> Class.lookup attr info
        (_, Object info)            -> Object.lookup attr info
        (_, Module info)            -> Module.lookup attr info
        _ -> do
            raise "TypeError" "object does not have attributes"
            return Nothing

    case (target, obj) of
        (Class info, Function name params body env) ->
            return $ Method name (ClassBinding (className info) target) params body env

        (Object info, Function name params body env) ->
            return $ Method name (InstanceBinding (className $ objectClass info) target) params body env
        _               -> return obj

isInstance :: Object -> ClassInfo -> Bool
isInstance (Object info) cls = objectClass info == cls || cls `elem` (classBases . objectClass $ info)
isInstance (Class info) cls = info == cls || cls `elem` classBases info
isInstance _ _ = False

len :: MonadInterpreter m => Object -> m Int
len obj@(Object {}) = do
    r <- invoke obj "__len__" []
    case r of
        (Int i) -> return $ fromIntegral i
        _       -> do
            raise "SystemError" "returned non-int from __len__ method"
            return 0

len (String s)  = return $ T.length s
len (Bytes b)   = return $ BS.length b
len (List ref)  = do
    l <- readRef ref
    return $ length l
len (Dict ref)  = do
    d <- readRef ref
    return $ IM.size d
len (Set ref)  = do
    s <- readRef ref
    return $ IM.size s
len _ = do
    raise "SystemError" "object has no __len__"
    return 0

setAttr :: (MonadInterpreter m) => Text -> Object -> Object -> m ()
setAttr attr obj target = case getObjAttrs target of
        Just ref -> do
            dict    <- readRef ref
            dict'   <- AttributeDict.set attr obj dict
            writeRef ref dict'
        Nothing -> raise "TypeError" "object does not have attributes"
