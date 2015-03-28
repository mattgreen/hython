module Hython.BuiltinTypes.Dict (dictPrimitives)
where

import Control.Monad.State
import Data.Hashable
import Data.IORef
import Data.HashMap.Strict as Map

import Hython.InterpreterState
import Hython.Object

dictPrimitives :: [(String, PrimitiveFn)]
dictPrimitives = [("dict-new", dictNew)
                 ,("dict-clear", dictClear)
                 ,("dict-contains", dictContains)
                 ,("dict-del", dictDel)
                 ,("dict-get", dictGet)
                 ,("dict-items", dictItems)
                 ,("dict-length", dictLength)
                 ,("dict-set", dictSet)
                 ]

dictNew :: PrimitiveFn
dictNew _ = do
    items <- newRef empty
    return $ Dict items

dictClear :: PrimitiveFn
dictClear [d@(Dict ref)] = do
    updateRef ref (const empty)
    return d

dictContains :: PrimitiveFn
dictContains [d@(Dict ref), k] = do
    dict <- liftIO $ readIORef ref
    case Map.lookup k dict of
        Just _  -> return $ Bool True
        Nothing -> return $ Bool False

dictDel :: PrimitiveFn
dictDel [d@(Dict ref), k] = do
    updateRef ref $ \m ->
        Map.delete k m
    return d

dictGet :: PrimitiveFn
dictGet [d@(Dict ref), k] = do
    dict <- liftIO $ readIORef ref
    case Map.lookup k dict of
        Just valueRef -> liftIO $ readIORef valueRef
        Nothing -> return None -- wrong

dictItems :: PrimitiveFn
dictItems [d@(Dict ref)] = do
    dict    <- liftIO $ readIORef ref
    items   <- mapM unwrap (Map.toList dict)
    return $ Tuple items
  where
    unwrap (k, vRef) = do
        v <- liftIO $ readIORef vRef
        return $ Tuple [k, v]

dictLength :: PrimitiveFn
dictLength [d@(Dict ref)] = do
    dict <- liftIO $ readIORef ref
    return $ Int (fromIntegral (Map.size dict))

dictSet :: PrimitiveFn
dictSet [d@(Dict ref), key, value] = do
    obj <- newRef value
    updateRef ref $ \m ->
        Map.insert key obj m
    return d

newRef v = liftIO $ newIORef v
updateRef ref action = liftIO $ modifyIORef' ref action
