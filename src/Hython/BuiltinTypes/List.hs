module Hython.BuiltinTypes.List
where

import qualified Data.Text as T
import Safe (atMay)

import Hython.Ref
import Hython.Types

listAppend :: MonadInterpreter m => ListRef -> Object -> m Object
listAppend ref obj = do
    modifyRef ref $ \l -> l ++ [obj]
    return None

listClear :: MonadInterpreter m => ListRef -> m Object
listClear ref = do
    writeRef ref []
    return None

listContains :: MonadInterpreter m => ListRef -> Object -> m Object
listContains ref obj = do
    l <- readRef ref
    results <- mapM (equal obj) l
    newBool $ True `elem` results

listConcat :: MonadInterpreter m => ListRef -> ListRef -> m Object
listConcat left right = do
    r <- readRef right
    modifyRef left $ \l -> l ++ r
    return None

listGet :: MonadInterpreter m => ListRef -> Object -> m Object
listGet ref (Int i) = do
    l <- readRef ref
    case atMay l (fromIntegral i) of
        Just obj    -> return obj
        Nothing     -> do
            raise "IndexError" "list index out of range"
            return None

listGet _ _ = do
    raise "TypeError" "list indicies must be integers"
    return None

listLength :: MonadInterpreter m => ListRef -> m Object
listLength ref = do
    l <- readRef ref
    newInt . fromIntegral . length $ l

listNew :: MonadInterpreter m => m Object
listNew = do
    r <- newRef []
    return $ List r
