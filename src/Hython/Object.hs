module Hython.Object (lookup)
where

import Prelude hiding (lookup)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (readIORef)
import Data.Text (Text)

import qualified Hython.AttributeDict as AttributeDict
import qualified Hython.Class as Class
import Hython.Types

lookup :: (MonadIO m) => Text -> ObjectInfo -> m (Maybe Object)
lookup attr info = do
    dict <- liftIO $ readIORef (objectDict info)
    result <- AttributeDict.lookup attr dict
    case result of
        Just obj -> return $ Just obj
        Nothing -> Class.lookup attr (objectClass info)
