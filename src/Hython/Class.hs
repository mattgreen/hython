module Hython.Class
where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.IORef (readIORef)

import qualified Hython.AttributeDict as AttributeDict
import Hython.Types

lookup :: (MonadIO m) => String -> ClassInfo -> m (Maybe Object)
lookup attr info = do
    dict <- liftIO $ readIORef $ classDict info
    AttributeDict.lookup attr dict
