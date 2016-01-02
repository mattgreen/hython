module Hython.BuiltinTypes.Dict
where

import qualified Data.IntMap as IntMap

import Hython.Ref
import Hython.Types

dictNew :: MonadInterpreter m => m Object
dictNew = do
    r <- newRef IntMap.empty
    return $ Dict r

dictClear :: MonadInterpreter m => DictRef -> m Object
dictClear ref = do
    writeRef ref IntMap.empty
    return None

dictContains :: MonadInterpreter m => DictRef -> Object -> m Object
dictContains ref obj = do
    dict    <- readRef ref
    key     <- hash obj
    newBool $ key `IntMap.member` dict

dictDel :: MonadInterpreter m => DictRef -> Object -> m Object
dictDel ref obj = do
    key <- hash obj
    modifyRef ref $ \m -> IntMap.delete key m
    return None

dictGet :: MonadInterpreter m => DictRef -> Object -> m Object
dictGet ref obj = do
    dict    <- readRef ref
    key     <- hash obj

    case IntMap.lookup key dict of
        Just (_, v)     -> return v
        Nothing         -> do
            raise "KeyError" "key not found"
            return None

dictItems :: MonadInterpreter m => DictRef -> m Object
dictItems ref = do
    dict    <- readRef ref
    items   <- mapM unwrap (IntMap.elems dict)
    newList items
  where
    unwrap (key, value) = newTuple [key, value]

dictLength :: MonadInterpreter m => DictRef -> m Object
dictLength ref = newInt . fromIntegral . IntMap.size =<< readRef ref

dictSet :: MonadInterpreter m => DictRef -> Object -> Object -> m Object
dictSet ref key value = do
    hashed  <- hash key
    modifyRef ref $ \m -> IntMap.insert hashed (key, value) m
    return None
