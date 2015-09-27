module Hython.Environment
where

import Prelude hiding (lookup)

import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import Safe (headDef)
import Hython.Name
import Hython.Types

bind :: MonadEnv m => Name -> Object -> m ()
bind name obj = do
    mref <- lookupRef name
    case mref of
        Just ref -> liftIO $ writeIORef ref obj
        Nothing -> do
            ref <- liftIO $ newIORef obj
            modifyEnv $ insertRef ref
  where
    insertRef ref env = case envFrames env of
        (e:es)  -> case Map.lookup name e of
            Just GlobalBinding    -> env { envModule = Map.insert name (LocalBinding ref) (envModule env) }
            Just NonlocalBinding    -> do
                let (before, x:xs) = break (Map.member name) es
                env { envFrames = before ++ [Map.insert name (LocalBinding ref) x] ++ xs }
            _                   -> env { envFrames = Map.insert name (LocalBinding ref) e : es }
        []      -> env { envModule = Map.insert name (LocalBinding ref) (envModule env) }

bindGlobal :: MonadEnv m => Name -> m ()
bindGlobal name = modifyEnv $ \env -> case envFrames env of
    (e:es)  -> env { envFrames = Map.insert name GlobalBinding e : es }
    []      -> env

bindNonlocal :: MonadEnv m => Name -> m (Either String ())
bindNonlocal name = do
    env <- getEnv
    case envFrames env of
        (e:es)  -> if any (Map.member name) es
            then do
                putEnv $ env { envFrames = Map.insert name NonlocalBinding e : es }
                return . Right $ ()
            else return . Left $ "no binding for nonlocal '" ++ unpack name ++ "' found"
        []  -> return . Left $ "nonlocal declaration not allowed at module level"

lookupName :: MonadEnv m => Name -> m (Maybe Object)
lookupName name = do
    mref <- lookupRef name
    case mref of
        Just ref    -> do
            obj <- liftIO . readIORef $ ref
            return $ Just obj
        Nothing     -> return Nothing

lookupRef :: MonadEnv m => Name -> m (Maybe ObjectRef)
lookupRef name = do
    env     <- getEnv
    scope   <- pure $ [headDef Map.empty (envFrames env)] ++ [envModule env] ++ [envBuiltins env]
    return $ lookupIn scope
  where
    lookupIn (e:es) = case Map.lookup name e of
        Just objRef -> case objRef of
            LocalBinding ref    -> Just ref
            _                   -> lookupIn es
        Nothing     -> lookupIn es
    lookupIn _     = Nothing

new :: [(Name, ObjectRef)] -> Env
new builtins = Env { envBuiltins = Map.fromList refs, envFrames = [], envModule = Map.empty }
  where
    refs = map (\x -> (fst x, LocalBinding (snd x))) builtins

pushEnvFrame :: (MonadEnv m) => m ()
pushEnvFrame = modifyEnv $ \env -> env { envFrames = Map.empty : envFrames env }

popEnvFrame :: (MonadEnv m) => m [(String, ObjectRef)]
popEnvFrame = do
    env <- getEnv
    case envFrames env of
        f:fs    -> do
            putEnv $ env { envFrames = fs }
            return . getLocals $ f
        []      -> return []
  where
    getLocals f = mapMaybe unwrap (Map.toList f)
    unwrap (name, LocalBinding ref) = Just (unpack name, ref)
    unwrap _ = Nothing

unbind :: MonadEnv m => Name -> m (Either String ())
unbind name = do
    env <- getEnv
    if any (Map.member name) (searchedFrames env)
        then do
            putEnv $ case envFrames env of
                (e:es)  -> env { envFrames = Map.delete name e : es }
                []      -> env { envModule = Map.delete name (envModule env) }
            return (Right ())
        else return . Left $ "name '" ++ unpack name ++ "' is not defined"
  where
    searchedFrames env = envFrames env ++ [envModule env]
