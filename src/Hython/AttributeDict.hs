module Hython.AttributeDict
where

import qualified Data.HashMap.Strict as Map
import Data.IORef

import Hython.Object

clone :: AttributeDict -> IO AttributeDict
clone dictRef = do
    dict <- readIORef dictRef
    contents <- mapM unwrap (Map.toList dict)
    fromList contents

  where
    unwrap (key, ref) = do
        value <- readIORef ref
        return (key, value)

delete :: String -> AttributeDict -> IO ()
delete key dictRef = do
    dict <- readIORef dictRef
    writeIORef dictRef $ Map.delete key dict

empty :: IO AttributeDict
empty = newIORef Map.empty

fromList :: [(String, Object)] -> IO AttributeDict
fromList list = do
    contents <- mapM wrap list
    newIORef $ Map.fromList contents
  where
    wrap (key, value) = do
        ref <- newIORef value
        return (key, ref)

lookup :: String -> AttributeDict -> IO (Maybe Object)
lookup key dictRef = do
    dict <- readIORef dictRef
    case Map.lookup key dict of
        Just valueRef -> do
            value <- readIORef valueRef
            return $ Just value
        Nothing -> return Nothing

union :: AttributeDict -> AttributeDict -> IO AttributeDict
union l r = do
    left    <- readIORef l
    mapM_ merge (Map.toList left)
    return r
  where
    merge (k,ref) = do
        v <- readIORef ref
        update k v r

update :: String -> Object -> AttributeDict -> IO ()
update key value dictRef = do
    dict <- readIORef dictRef
    case Map.lookup key dict of
        Just existing   -> modifyIORef' existing (const value)
        Nothing         -> do
            valueRef <- newIORef value
            modifyIORef' dictRef (Map.insert key valueRef)

