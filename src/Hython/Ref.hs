module Hython.Ref
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef

type Ref a = IORef a

newRef :: MonadIO m => a -> m (IORef a)
newRef obj = liftIO $ newIORef obj

readRef :: MonadIO m => IORef a -> m a
readRef ref = liftIO $ readIORef ref

modifyRef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyRef ref action = liftIO $ modifyIORef' ref action

writeRef :: MonadIO m => IORef a -> a -> m ()
writeRef ref obj = liftIO $ writeIORef ref obj
