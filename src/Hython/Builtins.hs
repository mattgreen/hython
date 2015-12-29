module Hython.Builtins where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T

import qualified Hython.AttributeDict as AttributeDict
import qualified Hython.Class as Class
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

    ("len", [obj]) -> case obj of
        Object {}   -> invoke obj "__len__" []
        (String s)  -> newInt . toInteger $ T.length s
        (Bytes b)   -> newInt . toInteger $ BS.length b
        (Tuple t)   -> newInt . toInteger $ length t
        (List ref)  -> do
            l <- readRef ref
            newInt . toInteger . length $ l
        (Dict ref)  -> do
            d <- readRef ref
            newInt . toInteger . IM.size $ d
        (Set ref)  -> do
            s <- readRef ref
            newInt . toInteger . IM.size $ s
        _ -> ignore $ raise "SystemError" "object has no __len__"
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
isInstance (Object info) cls = objectClass info == cls || cls `elem` (classBases . objectClass $ info)
isInstance (Class info) cls = info == cls || cls `elem` classBases info
isInstance _ _ = False

asStr :: MonadInterpreter m => Object -> m String
asStr (String s)    = return . T.unpack $ s
asStr o@_           = toStr o

setAttr :: (MonadInterpreter m) => Text -> Object -> Object -> m ()
setAttr attr obj target = case getObjAttrs target of
        Just ref -> do
            dict    <- readRef ref
            dict'   <- AttributeDict.set attr obj dict
            writeRef ref dict'
        Nothing -> raise "TypeError" "object does not have attributes"
