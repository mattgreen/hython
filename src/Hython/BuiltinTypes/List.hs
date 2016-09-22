module Hython.BuiltinTypes.List
where

import Control.Monad (when)

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
listGet ref iObj = do
    l <- readRef ref
    i <- convertAndCheckIndex iObj l
    return $ l !! i

listLength :: MonadInterpreter m => ListRef -> m Object
listLength ref = do
    l <- readRef ref
    newInt . fromIntegral . length $ l

listNew :: MonadInterpreter m => m Object
listNew = do
    r <- newRef []
    return $ List r

listSet :: MonadInterpreter m => ListRef -> Object -> Object -> m Object
listSet ref iObj v = do
    l <- readRef ref
    i <- convertAndCheckIndex iObj l
    modifyRef ref $ \list -> take i list ++ [v] ++ drop (i + 1) list
    return None


-- returns the converted index if valid, raises (Python) exception otherwise
convertAndCheckIndex :: MonadInterpreter m => Object -> [a] -> m Int
convertAndCheckIndex (Int i) l = do
    let index = fromIntegral i
    when (index < 0 || index >= length l)
        (raise "IndexError" "list index out of range")
    return index

convertAndCheckIndex (Bool b) l = convertAndCheckIndex int l
  where int = Int . toInteger . fromEnum $ b

convertAndCheckIndex _ _ = do
    raise "TypeError" "list indices must be integers"
    return $ error "irrelevant after raising exception"
