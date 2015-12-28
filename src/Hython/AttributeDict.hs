module Hython.AttributeDict
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Hython.Types

lookup :: (MonadIO m) => Text -> AttributeDict -> m (Maybe Object)
lookup attr dict = case HashMap.lookup attr dict of
                       Just ref -> Just <$> readRef ref
                       Nothing  -> return Nothing

new :: AttributeDict
new = HashMap.empty

set :: (MonadIO m) => Text -> Object -> AttributeDict -> m AttributeDict
set attr obj dict = case HashMap.lookup attr dict of
                        Just ref -> do
                            writeRef ref obj
                            return dict
                        Nothing -> do
                            ref <- newRef obj
                            return $ HashMap.insert attr ref dict
