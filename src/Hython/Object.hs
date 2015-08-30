module Hython.Object
where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.Hashable as H
import qualified Data.ByteString.Char8 as B
import Data.Complex (Complex, realPart, imagPart)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (intercalate)

import Language.Python (Param, Statement)

import Hython.Name

data Object = None
            | Bool Bool
            | Bytes ByteString
            | Float Double
            | Imaginary (Complex Double)
            | Int Integer
            | String String
            | List (IORef [Object])
            | Dict (IORef (IntMap (Object, Object)))
            | Set (IORef (IntMap Object))
            | Tuple [Object]
            | BuiltinFn String
            | Function String [FnParam] [Statement]

data FnParam = NamedParam String
             | DefParam String Object
             | SParam String
             | DSParam String

type ObjectRef = IORef Object

class Monad m => MonadEnvironment m where
    bind            :: Name -> Object -> m ()
    bindGlobal      :: Name -> m ()
    bindNonlocal    :: Name -> m ()
    lookupName      :: Name -> m (Maybe Object)
    pushEnvFrame    :: m ()
    popEnvFrame     :: m ()
    unbind          :: Name -> m ()

class MonadEnvironment m => MonadInterpreter m where
    evalBlock       :: [Statement] -> m ()
    getControlCont  :: ControlCont -> m (Maybe (Object -> m ()))
    popControlCont  :: ControlCont -> m ()
    pushControlCont :: ControlCont -> (Object -> m ()) -> m ()
    pushEvalResult  :: Object -> m ()
    raise           :: String -> String -> m ()

data ControlCont = BreakCont
                 | ContinueCont
                 | ReturnCont

hash :: (MonadInterpreter m, MonadIO m) => Object -> m Int
hash obj = case obj of
    (None)          -> return 0
    (Bool b)        -> return $ H.hash b
    (BuiltinFn b)   -> return $ H.hash b
    (Bytes b)       -> return $ H.hash b
    (Float d)       -> return $ H.hash d
    (Imaginary i)   -> return $ H.hash (realPart i) + H.hash (imagPart i)
    (Int i)         -> return $ H.hash i
    (String s)      -> return $ H.hash s
    (Tuple items)   -> do
        hashes <- mapM hash items
        return $ sum hashes
    _               -> do
        raise "TypeError" "unhashable type"
        return 0

newNone :: MonadInterpreter m => m Object
newNone = return None

newBool :: MonadInterpreter m => Bool -> m Object
newBool b = return $ Bool b

newBytes :: MonadInterpreter m => String -> m Object
newBytes b = return $ Bytes (B.pack b)

newDict :: (MonadInterpreter m, MonadIO m) => [(Object, Object)] -> m Object
newDict objs = do
    items <- forM objs $ \p@(key, _) -> do
        h <- hash key
        return (h, p)
    ref <- liftIO $ newIORef (IntMap.fromList items)
    return $ Dict ref

newFloat :: MonadInterpreter m => Double -> m Object
newFloat d = return $ Float d

newFunction :: MonadInterpreter m => String -> [FnParam] -> [Statement] -> m Object
newFunction name params block = return $ Function name params block

newImag :: MonadInterpreter m => Complex Double -> m Object
newImag i = return $ Imaginary i

newInt :: MonadInterpreter m => Integer -> m Object
newInt i = return $ Int i

newList :: (MonadInterpreter m, MonadIO m) => [Object] -> m Object
newList l = do
    ref <- liftIO $ newIORef l
    return $ List ref

newSet :: (MonadInterpreter m, MonadIO m) => [Object] -> m Object
newSet objs = do
    items <- forM objs $ \obj -> do
        key <- hash obj
        return (key, obj)
    ref <- liftIO $ newIORef (IntMap.fromList items)
    return $ Set ref

newString :: MonadInterpreter m => String -> m Object
newString s = return $ String s

newTuple :: MonadInterpreter m => [Object] -> m Object
newTuple l = return $ Tuple l

isNone :: Object -> Bool
isNone (None) = True
isNone _ = False

isTruthy :: MonadIO m => Object -> m Bool
isTruthy (None) = return False
isTruthy (Bool False) = return False
isTruthy (Int 0) = return False
isTruthy (Float 0.0) = return False
isTruthy (String "") = return False
isTruthy (Bytes b) = return $ not (B.null b)
isTruthy (List ref) = do
    l <- liftIO $ readIORef ref
    return $ not (null l)
isTruthy (Tuple objs) = return $ not $ null objs
isTruthy _ = return True

toStr :: MonadIO m => Object -> m String
toStr (None) = return "None"
toStr (Bool b) = return $ if b then "True" else "False"
toStr (Bytes _b) = return "b'??'"
toStr (Float f) = return $ show f
toStr (Function name _ _) = return name
toStr (Imaginary i) = return $ show i
toStr (Int i) = return $ show i
toStr (String s) = return $ "'" ++ s ++ "'"
toStr (List ref) = do
    l <- liftIO $ readIORef ref
    strItems <- mapM toStr l
    return $ "[" ++ intercalate ", " strItems ++ "]"
toStr (Tuple objs) = do
    strItems <- mapM toStr objs
    case strItems of
        [str]   -> return $ "(" ++ str ++ ",)"
        _       -> return $ "(" ++ intercalate ", " strItems ++ ")"
toStr (Set ref) = do
    items <- liftIO $ readIORef ref
    strItems <- mapM toStr $ IntMap.elems items
    return $ "{" ++ intercalate ", " strItems ++ "}"
toStr (Dict ref) = do
    items <- liftIO $ readIORef ref
    strItems <- forM (IntMap.elems items) $ \(k, v) -> do
        key     <- toStr k
        value   <- toStr v
        return $ key ++ ": " ++ value
    return $ "{" ++ intercalate ", " strItems ++ "}"
toStr (BuiltinFn name)  = return $ "<built-in function " ++ name ++ ">"
