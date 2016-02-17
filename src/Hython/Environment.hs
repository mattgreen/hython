{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Hython.Environment
    ( MonadEnv
    , Environment
    , bind
    , bindNonlocal
    , bindGlobal
    , getClosingEnv
    , getEnv
    , moveLocalsToBuiltins
    , lookupName
    , new
    , pushModuleEnv
    , putEnv
    , putEnvWithBindings
    , restoreEnv
    , unbind
    )
where

import Prelude hiding (lookup)

import qualified Control.Arrow as Arrow
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Monoid

import Hython.Name
import Hython.Ref

class MonadIO m => MonadEnv obj m | m -> obj where
    getEnv          :: m (Environment obj)
    putEnv          :: Environment obj -> m ()
    modifyEnv       :: (Environment obj -> Environment obj) -> m ()
    modifyEnv action = do
        env <- getEnv
        putEnv $ action env

data Environment obj = Environment
    { localEnv      :: Ref (EnvMap obj)
    , enclosingEnvs :: [Ref (EnvMap obj)]
    , moduleEnv     :: Ref (EnvMap obj)
    , builtinEnv    :: Ref (EnvMap obj)
    , activeEnv     :: ActiveEnv
    }

data ActiveEnv
    = ModuleEnv
    | LocalEnv
    deriving Eq

type EnvMap obj = HashMap Name (Binding obj)

data Binding obj
    = LocalBinding (Ref obj)
    | NonlocalBinding
    | ModuleBinding

bind :: MonadEnv obj m => Name -> obj -> m ()
bind name obj = do
    envMap  <- readRef . localEnv =<< getEnv
    mref    <- lookupRefIn name envMap
    case mref of
        Just ref    -> writeRef ref obj
        Nothing     -> do
            ref <- newRef obj
            modifyLocalEnv $ Map.insert name (LocalBinding ref)

bindGlobal :: MonadEnv obj m => Name -> m ()
bindGlobal name = do
    env <- getEnv
    case activeEnv env of
        LocalEnv    -> modifyLocalEnv $ Map.insert name ModuleBinding
        ModuleEnv   -> return ()

bindNonlocal :: MonadEnv obj m => Name -> m (Either String ())
bindNonlocal name = do
    env <- getEnv
    case activeEnv env of
        LocalEnv -> do
            mref <- lookupRefInEnclosing name
            case mref of
                Just _  -> do
                    modifyLocalEnv $ Map.insert name NonlocalBinding
                    return $ Right ()
                Nothing -> return $ Left ("no binding for nonlocal '" ++ show name ++ "' found")

        ModuleEnv -> return $ Left "nonlocal declaration not allowed at module level"

getClosingEnv :: MonadEnv obj m => m (Environment obj)
getClosingEnv = do
    env <- getEnv
    return $ case activeEnv env of
        LocalEnv    -> env { enclosingEnvs = localEnv env : enclosingEnvs env }
        ModuleEnv   -> env

moveLocalsToBuiltins :: (MonadIO m, MonadEnv obj m) => m ()
moveLocalsToBuiltins = do
    locals <- readRef =<< localEnv <$> getEnv

    modifyBuiltinEnv $ Map.union locals
    modifyLocalEnv $ const Map.empty

lookupName :: MonadEnv obj m => Name -> m (Maybe obj)
lookupName name = do
    mref <- lookupRef name
    case mref of
        Just ref    -> return . Just =<< readRef ref
        Nothing     -> return Nothing

lookupRef :: MonadEnv obj m => Name -> m (Maybe (Ref obj))
lookupRef name = do
    env <- getEnv
    search $ [localEnv env] ++ enclosingEnvs env ++ [moduleEnv env, builtinEnv env]
  where
    search (r:rs) = do
        envMap  <- readRef r
        mref    <- lookupRefIn name envMap
        case mref of
            Just ref    -> return $ Just ref
            Nothing     -> search rs
    search [] = return Nothing

lookupRefIn :: MonadEnv obj m => Name -> EnvMap obj -> m (Maybe (Ref obj))
lookupRefIn name envMap = do
    env <- getEnv
    case Map.lookup name envMap of
        Just (LocalBinding ref) -> return $ Just ref
        Just NonlocalBinding    -> lookupRefInEnclosing name
        Just ModuleBinding      -> do
            moduleMap   <- readRef $ moduleEnv env
            lookupRefIn name moduleMap
        Nothing                 -> return Nothing

lookupRefInEnclosing :: MonadEnv obj m => Name -> m (Maybe (Ref obj))
lookupRefInEnclosing name = do
    envMaps <- mapM readRef =<< enclosingEnvs <$> getEnv
    results <- mapM (lookupRefIn name) envMaps
    return . getFirst . mconcat $ map First results

new :: [(Name, Ref obj)] -> IO (Environment obj)
new builtins = do
    moduleRef   <- newRef Map.empty
    builtinRef  <- newRef $ Map.fromList (map (Arrow.second LocalBinding) builtins)

    return Environment
        { localEnv = moduleRef
        , enclosingEnvs = []
        , moduleEnv = moduleRef
        , builtinEnv = builtinRef
        , activeEnv = ModuleEnv
        }

pushModuleEnv action = do
    prev    <- getEnv
    ref     <- newRef Map.empty
    modifyEnv $ \env -> env { localEnv = ref, moduleEnv = ref, activeEnv = ModuleEnv }
    action
    restoreEnv prev

restoreEnv :: MonadEnv obj m => Environment obj -> m [(Name, Ref obj)]
restoreEnv env = do
    bindings <- readRef . localEnv =<< getEnv
    putEnv env
    return $ mapMaybe unwrap (Map.toList bindings)
  where
    unwrap (n, LocalBinding r) = Just (n, r)
    unwrap _ = Nothing

putEnvWithBindings :: MonadEnv obj m => [(Name, obj)] -> Environment obj -> m (Environment obj)
putEnvWithBindings bindings env = do
    prev    <- getEnv
    refList <- mapM wrap bindings
    ref     <- newRef . Map.fromList $ refList
    putEnv env { localEnv = ref, activeEnv = LocalEnv }
    return prev
  where
    wrap (n, o) = do
        r <- newRef o
        return (n, LocalBinding r)

unbind :: MonadEnv obj m => Name -> m (Either String ())
unbind name = do
    envMap  <- readRef . localEnv =<< getEnv

    case Map.lookup name envMap of
        Just binding    -> do
            modifyLocalEnv $ Map.delete name
            case binding of
                NonlocalBinding -> modifyEnclosingEnvs $ Map.delete name
                ModuleBinding   -> modifyModuleEnv $ Map.delete name
                _               -> return ()
            return $ Right ()
        Nothing -> return . Left $ "name '" ++ show name ++ "' is not defined"

modifyEnvMap :: MonadEnv obj m => (Environment obj -> Ref b) -> (b -> b) -> m ()
modifyEnvMap f action = do
    env <- getEnv
    modifyRef (f env) action

modifyEnclosingEnvs :: MonadEnv obj m => (EnvMap obj -> EnvMap obj) -> m ()
modifyEnclosingEnvs action = do
    env <- getEnv
    forM_ (enclosingEnvs env) (`modifyRef` action)

modifyModuleEnv :: MonadEnv obj m => (EnvMap obj -> EnvMap obj) -> m ()
modifyModuleEnv = modifyEnvMap moduleEnv

modifyLocalEnv :: MonadEnv obj m => (EnvMap obj -> EnvMap obj) -> m ()
modifyLocalEnv = modifyEnvMap localEnv

modifyBuiltinEnv :: MonadEnv obj m => (EnvMap obj -> EnvMap obj) -> m ()
modifyBuiltinEnv = modifyEnvMap builtinEnv

