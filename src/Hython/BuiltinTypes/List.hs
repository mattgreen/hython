module Hython.BuiltinTypes.List
where

import Control.Monad.State
import Data.IORef

import Hython.InterpreterState
import Hython.Object

listNew :: PrimitiveFn
listNew _ = do
    items <- liftIO $ newIORef []
    return $ List items

listAppend :: PrimitiveFn
listAppend [l@(List ref), obj] = do
    liftIO $ modifyIORef' ref (\l -> l ++ [obj])
    return l

listConcat :: PrimitiveFn
listConcat [l@(List lRef), List rRef] = do
    items <- liftIO $ readIORef rRef
    liftIO $ modifyIORef' lRef (++ items)
    return l

listClear :: PrimitiveFn
listClear [l@(List ref)] = do
    liftIO $ modifyIORef' ref (const [])
    return l

listLength :: PrimitiveFn
listLength [List ref] = do
    items <- liftIO $ readIORef ref
    return $ Int (fromIntegral (length items))

