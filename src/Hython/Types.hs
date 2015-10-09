{-# LANGUAGE FlexibleContexts #-}

module Hython.Types
where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.Hashable as H
import qualified Data.ByteString.Char8 as B
import Data.Complex (Complex, realPart, imagPart)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (intercalate)

import Language.Python (Statement)

import Hython.ControlFlow
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
            | Method String MethodBinding [FnParam] [Statement]
            | Class ClassInfo
            | Object ObjectInfo

type ObjectRef = IORef Object

data MethodBinding = ClassBinding String Object
                   | InstanceBinding String Object

data FnParam = NamedParam String
             | DefParam String Object
             | SParam String
             | DSParam String

data ClassInfo = ClassInfo
               { className  :: String
               , classBases :: [ClassInfo]
               , classDict  :: IORef AttributeDict
               } deriving (Eq)

data ObjectInfo = ObjectInfo
                { objectClass :: ClassInfo
                , objectDict :: IORef AttributeDict
                }

type AttributeDict = HashMap String (IORef Object)

data Env = Env
    { envModule     :: HashMap Name Binding
    , envBuiltins   :: HashMap Name Binding
    , envFrames     :: [HashMap Name Binding]
    }

data Binding
    = LocalBinding ObjectRef
    | NonlocalBinding
    | GlobalBinding

class HasAttributes a where
    getObjAttrs :: a -> Maybe (IORef AttributeDict)

instance HasAttributes Object where
    getObjAttrs (Object info) = Just $ objectDict info
    getObjAttrs (Class info) = Just $ classDict info
    getObjAttrs _ = Nothing

class MonadIO m => MonadEnv m where
    getEnv          :: m Env
    putEnv          :: Env -> m ()
    modifyEnv       :: (Env -> Env) -> m ()
    modifyEnv action = do
        env <- getEnv
        putEnv $ action env

class (MonadEnv m, MonadFlow (Object -> m ()) m, MonadIO m) => MonadInterpreter m where
    evalBlock       :: [Statement] -> m ()
    pushEvalResult  :: Object -> m ()
    raise           :: String -> String -> m ()

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

newClass :: (MonadIO m) => String -> [ClassInfo] -> [(String, ObjectRef)] -> m Object
newClass name bases dict = do
    ref <- liftIO $ newIORef $ HashMap.fromList dict
    classInfo <- pure ClassInfo {
        className = name,
        classBases = bases,
        classDict = ref
    }

    return $ Class classInfo

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

newObject :: (MonadIO m) => ClassInfo -> m Object
newObject cls = do
    clsDict <- liftIO $ readIORef (classDict cls)
    dict <- liftIO $ newIORef clsDict
    info <- pure ObjectInfo {
        objectClass = cls,
        objectDict = dict
    }

    return $ Object info

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
toStr (Imaginary i)
    | realPart i == 0   = return $ show i
    | otherwise         = return $ show i
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
toStr (Class info) = return $ "<class '" ++ className info ++ "'>"
toStr (Object info) = return $ "<" ++ className (objectClass info) ++ " object>"
toStr (Method name (ClassBinding clsName _) _ _) = 
    return $ "<method '" ++ name ++ "' of '" ++ clsName ++ "' objects>"
toStr (Method name (InstanceBinding clsName obj) _ _) = do
    s <- toStr obj
    return $ "<bound method " ++ clsName ++ "." ++ name ++ " of " ++ s ++ ">"
