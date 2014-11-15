module Hython.Attributes
where

import qualified Data.HashMap.Strict as Map
import Data.IORef

import Language.Python.Core

cloneAttributeDict :: AttributeDict -> IO AttributeDict
cloneAttributeDict ref = do
    dict <- readIORef ref
    contents <- mapM unwrap (Map.toList dict)
    newAttributeDict contents

  where
    unwrap (key, ref) = do
        value <- readIORef ref
        return (key, value)

newAttributeDict :: [(String, Value)] -> IO AttributeDict
newAttributeDict values = do
    contents <- mapM wrap values
    newIORef $ Map.fromList contents

  where
    wrap (key,value) = do
        ref <- newIORef value
        return (key, ref)

readAttr :: String -> AttributeDict -> IO (Maybe Value)
readAttr attr ref = do
    dict <- readIORef ref
    case Map.lookup attr dict of
        Just valueRef -> do
            value <- readIORef valueRef
            return $ Just value
        Nothing -> return Nothing

removeAttr :: String -> AttributeDict -> IO ()
removeAttr attr ref = modifyIORef ref (Map.delete attr)

writeAttr :: String -> Value -> AttributeDict -> IO ()
writeAttr attr value ref = do
    dict <- readIORef ref
    case Map.lookup attr dict of
        Just existing   -> modifyIORef' existing (const value)
        Nothing         -> do
            valueRef <- newIORef value
            modifyIORef' ref (Map.insert attr valueRef)

