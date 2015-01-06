module Hython.NameResolution
where

import qualified Hython.AttributeDict as AttributeDict
import Hython.Object

getActiveEnv :: Env -> AttributeDict
getActiveEnv env = if activeEnv env == ModuleEnv
    then moduleEnv env
    else localEnv env

bindGlobalName :: String -> Env -> IO ()
bindGlobalName name env = do
    ref <- AttributeDict.lookupRef name (moduleEnv env)
    case ref of
        Just r  -> AttributeDict.updateRef name r (localEnv env)
        Nothing -> return ()

bindName :: String -> Object -> Env -> IO ()
bindName name object env = AttributeDict.update name object dict
  where
    dict = getActiveEnv env

bindNames :: AttributeDict -> Env -> IO ()
bindNames names env = do
    -- TODO: fix AttributeDict to not be so weird
    _ <- AttributeDict.union names dict
    return ()
  where
    dict = getActiveEnv env

bindNonlocalName :: String -> Env -> IO Bool
bindNonlocalName name env = do
    maybeRef <- lookupIn (enclosingEnvs env)
    case maybeRef of
        Just ref -> do
            AttributeDict.updateRef name ref (localEnv env)
            return True
        Nothing -> return False
  where
    lookupIn (d:ds) = do
        ref <- AttributeDict.lookupRef name d
        case ref of
            Just r  -> return $ Just r
            Nothing -> lookupIn ds
    lookupIn [] = return Nothing

lookupName :: String -> Env -> IO (Maybe Object)
lookupName name env = lookupIn envs
  where
    lookupIn (d:ds) = do
        obj <- AttributeDict.lookup name d
        case obj of
            Just _  -> return obj
            Nothing -> lookupIn ds
    lookupIn [] = return Nothing

    envs = case activeEnv env of
        ModuleEnv   -> [moduleEnv env, builtinEnv env]
        _           -> [localEnv env] ++ enclosingEnvs env ++ [moduleEnv env, builtinEnv env]

unbindName :: String -> Env -> IO ()
unbindName name env = AttributeDict.delete name dict
  where
    dict = getActiveEnv env
