module Hython.NameResolution
where

import Language.Python.Core
import qualified Hython.AttributeDict as AttributeDict
import Hython.InterpreterState

getActiveScope :: Scope -> AttributeDict
getActiveScope scope = do
    if activeScope scope == ModuleScope
        then moduleScope scope
        else localScope scope

bindName :: String -> Object -> Scope -> IO Scope
bindName name object scope = do
    AttributeDict.update name object dict
    return $ scope
  where
    dict = getActiveScope scope

lookupName :: String -> Scope -> IO (Maybe Object)
lookupName name scope = do
    result <- lookup scopes
    return result
  where
    lookup :: [AttributeDict] -> IO (Maybe Object)
    lookup (d:ds) = do
        obj <- AttributeDict.lookup name d
        case obj of
            Just _  -> return obj
            Nothing -> lookup ds
    lookup [] = return Nothing

    scopes = case activeScope scope of
        ModuleScope -> [moduleScope scope, builtinScope scope]
        _           -> [localScope scope, moduleScope scope, builtinScope scope]

unbindName :: String -> Scope -> IO Scope
unbindName name scope = do
    AttributeDict.delete name dict
    return scope
  where
    dict = getActiveScope scope
