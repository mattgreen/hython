module Hython.NameResolution
where

import Language.Python.Core
import qualified Hython.AttributeDict as AttributeDict
import Hython.InterpreterState

getActiveScope :: Scope -> AttributeDict
getActiveScope scope = if activeScope scope == ModuleScope
    then moduleScope scope
    else localScope scope

bindName :: String -> Object -> Scope -> IO ()
bindName name object scope = AttributeDict.update name object dict
  where
    dict = getActiveScope scope

bindNames :: AttributeDict -> Scope -> IO ()
bindNames names scope = do
    -- TODO: fix AttributeDict to not be so weird
    _ <- AttributeDict.union names dict
    return ()
  where
    dict = getActiveScope scope

lookupName :: String -> Scope -> IO (Maybe Object)
lookupName name scope = lookupIn scopes
  where
    lookupIn (d:ds) = do
        obj <- AttributeDict.lookup name d
        case obj of
            Just _  -> return obj
            Nothing -> lookupIn ds
    lookupIn [] = return Nothing

    scopes = case activeScope scope of
        ModuleScope -> [moduleScope scope, builtinScope scope]
        _           -> [localScope scope, moduleScope scope, builtinScope scope]

unbindName :: String -> Scope -> IO ()
unbindName name scope = AttributeDict.delete name dict
  where
    dict = getActiveScope scope
