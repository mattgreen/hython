{-# LANGUAGE RankNTypes, KindSignatures, MultiParamTypeClasses, FunctionalDependencies #-}

module Hython.Environment
    ( MonadEnv
    , Environment
    , bind
    , bindNonlocal
    , bindGlobal
    , bindMany
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

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Monoid

import Hython.AttributeDict (AttributeDict)
import qualified Hython.AttributeDict as AttributeDict
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
    { localEnv      :: Ref (LocalEnv obj)
    , enclosingEnvs :: [Ref (LocalEnv obj)]
    , moduleEnv     :: Ref (AttributeDict obj)
    , builtinEnv    :: Ref (AttributeDict obj)
    , activeEnv     :: ActiveEnv
    }

data ActiveEnv
    = ModuleEnv
    | LocalEnv
    deriving Eq

type LocalEnv obj = HashMap Name (Binding obj)

data Binding obj
    = LocalBinding (Ref obj)
    | NonlocalBinding
    | ModuleBinding

bind :: MonadEnv obj m => Name -> obj -> m ()
bind name obj = do
    env     <- getEnv
    envMap  <- readRef . localEnv $ env
    mref    <- lookupRefIn name envMap
    case mref of
        Just ref    -> writeRef ref obj
        Nothing     -> do
            ref <- newRef obj
            case activeEnv env of
                ModuleEnv   -> modifyModuleEnv $ AttributeDict.insertRef name ref
                LocalEnv    -> modifyLocalEnv $ Map.insert name (LocalBinding ref)

bindMany :: MonadEnv obj m => [(Name, Ref obj)] -> m ()
bindMany vars = forM_ vars $ \(name, ref) -> do
    obj <- readRef ref
    bind name obj

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
    moduleVars <- readRef =<< moduleEnv <$> getEnv

    modifyBuiltinEnv $ Map.union moduleVars
    modifyModuleEnv $ const Map.empty

lookupName :: MonadEnv obj m => Name -> m (Maybe obj)
lookupName name = do
    mref <- lookupRef name
    case mref of
        Just ref    -> return . Just =<< readRef ref
        Nothing     -> return Nothing

lookupRef :: MonadEnv obj m => Name -> m (Maybe (Ref obj))
lookupRef name = do
    env <- getEnv
    mresult <- searchLocals $ localEnv env : enclosingEnvs env
    case mresult of
        r@(Just _)  -> return r
        Nothing     -> search [moduleEnv env, builtinEnv env]
  where
    searchLocals (r:rs) = do
        envMap  <- readRef r
        mref    <- lookupRefIn name envMap
        case mref of
            Just ref    -> return $ Just ref
            Nothing     -> searchLocals rs
    searchLocals [] = return Nothing

    search (r:rs) = do
        envMap  <- readRef r
        case AttributeDict.lookupRef name envMap of
            Just ref    -> return $ Just ref
            Nothing     -> search rs

    search [] = return Nothing

lookupRefIn :: MonadEnv obj m => Name -> LocalEnv obj -> m (Maybe (Ref obj))
lookupRefIn name envMap = do
    env <- getEnv
    case Map.lookup name envMap of
        Just (LocalBinding ref) -> return $ Just ref
        Just NonlocalBinding    -> lookupRefInEnclosing name
        Just ModuleBinding      -> do
            moduleMap   <- readRef $ moduleEnv env
            return $ AttributeDict.lookupRef name moduleMap
        Nothing                 -> return Nothing

lookupRefInEnclosing :: MonadEnv obj m => Name -> m (Maybe (Ref obj))
lookupRefInEnclosing name = do
    envMaps <- mapM readRef =<< enclosingEnvs <$> getEnv
    results <- mapM (lookupRefIn name) envMaps
    return . getFirst . mconcat $ map First results

new :: [(Name, Ref obj)] -> IO (Environment obj)
new builtins = do
    ref         <- newRef Map.empty
    moduleRef   <- newRef AttributeDict.empty
    builtinRef  <- newRef $ AttributeDict.fromList builtins

    return Environment
        { localEnv = ref
        , enclosingEnvs = []
        , moduleEnv = moduleRef
        , builtinEnv = builtinRef
        , activeEnv = ModuleEnv
        }

pushModuleEnv :: forall (m :: * -> *) obj a . MonadEnv obj m => Ref (AttributeDict obj) -> m a -> m ()
pushModuleEnv moduleRef action = do
    prev    <- getEnv
    ref     <- newRef Map.empty
    modifyEnv $ \env -> env { localEnv = ref, enclosingEnvs = [], moduleEnv = moduleRef, activeEnv = ModuleEnv }
    void action
    void $ restoreEnv prev
    return ()

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
    env <- getEnv
    case activeEnv env of
        LocalEnv -> do
            envMap  <- readRef . localEnv $ env

            case Map.lookup name envMap of
                Just binding    -> do
                    modifyLocalEnv $ Map.delete name
                    case binding of
                        NonlocalBinding -> modifyEnclosingEnvs $ Map.delete name
                        ModuleBinding   -> modifyModuleEnv $ Map.delete name
                        _               -> return ()
                    return $ Right ()
                Nothing -> return . Left $ "name '" ++ show name ++ "' is not defined"
        ModuleEnv -> do
            envMap  <- readRef . moduleEnv $ env

            case AttributeDict.lookupRef name envMap of
                Just _  -> do
                    modifyModuleEnv $ Map.delete name
                    return $ Right ()
                Nothing -> return . Left $ "name '" ++ show name ++ "' is not defined"

modifyEnclosingEnvs :: MonadEnv obj m => (LocalEnv obj -> LocalEnv obj) -> m ()
modifyEnclosingEnvs action = do
    env <- getEnv
    forM_ (enclosingEnvs env) (`modifyRef` action)

modifyModuleEnv :: MonadEnv obj m => (AttributeDict obj -> AttributeDict obj) -> m ()
modifyModuleEnv action = do
    env <- getEnv
    modifyRef (moduleEnv env) action

modifyLocalEnv :: MonadEnv obj m => (LocalEnv obj -> LocalEnv obj) -> m ()
modifyLocalEnv action = do
    env <- getEnv
    modifyRef (localEnv env) action

modifyBuiltinEnv :: MonadEnv obj m => (AttributeDict obj -> AttributeDict obj) -> m ()
modifyBuiltinEnv action = do
    env <- getEnv
    modifyRef (builtinEnv env) action

