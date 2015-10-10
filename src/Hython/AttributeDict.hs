module Hython.AttributeDict
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Hython.Types

lookup :: (MonadIO m) => Text -> AttributeDict -> m (Maybe Object)
lookup attr dict = case HashMap.lookup attr dict of
                       Just ref -> liftIO $ Just <$> readIORef ref
                       Nothing  -> return Nothing

new :: AttributeDict
new = HashMap.empty

set :: (MonadIO m) => Text -> Object -> AttributeDict -> m AttributeDict
set attr obj dict = case HashMap.lookup attr dict of
                        Just ref -> do
                            liftIO $ writeIORef ref obj
                            return dict
                        Nothing -> do
                            ref <- liftIO $ newIORef obj
                            return $ HashMap.insert attr ref dict
