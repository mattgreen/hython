{-# LANGUAGE FlexibleContexts #-}

module Hython.Types
where

import Control.Monad (forM, forM_, zipWithM)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.Hashable as H
import qualified Data.ByteString.Char8 as B
import Data.Complex (Complex, realPart, imagPart)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T

import Language.Python (Statement)

import Hython.ControlFlow (MonadFlow)
import Hython.Name
import Hython.Ref

data Object = None
            | Bool Bool
            | Bytes ByteString
            | Float Double
            | Imaginary (Complex Double)
            | Int Integer
            | String Text
            | List ListRef
            | Dict DictRef
            | Set SetRef
            | Tuple [Object]
            | BuiltinFn Text
            | Function Text [FnParam] [Statement]
            | Method Text MethodBinding [FnParam] [Statement]
            | Class ClassInfo
            | Object ObjectInfo

type ObjectRef = Ref Object

data MethodBinding = ClassBinding Text Object
                   | InstanceBinding Text Object

data FnParam = NamedParam Text
             | DefParam Text Object
             | SParam Text
             | DSParam Text

data ClassInfo = ClassInfo
               { className  :: Text
               , classBases :: [ClassInfo]
               , classDict  :: Ref AttributeDict
               }

instance Eq ClassInfo where
    l == r = className l == className r

data ObjectInfo = ObjectInfo
                { objectClass :: ClassInfo
                , objectDict :: Ref AttributeDict
                }

type DictRef    = Ref (IntMap (Object, Object))
type ListRef    = Ref [Object]
type SetRef     = Ref (IntMap Object)

type AttributeDict = HashMap Text (Ref Object)

data Env = Env
    { envModule     :: HashMap Name Binding
    , envBuiltins   :: HashMap Name Binding
    , envFrames     :: [HashMap Name Binding]
    }

data Binding
    = LocalBinding ObjectRef
    | NonlocalBinding
    | GlobalBinding

data ExceptionHandler = ExceptionHandler Name ClassInfo [Statement]

class HasAttributes a where
    getObjAttrs :: a -> Maybe (Ref AttributeDict)

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

class (MonadEnv m, MonadFlow Object (Object -> m ()) m, MonadIO m) => MonadInterpreter m where
    evalBlock       :: [Statement] -> m ()
    invoke          :: Object -> String -> [Object] -> m Object
    new             :: String -> [Object] -> m Object
    pushEvalResult  :: String -> m ()
    raise           :: String -> String -> m ()

hash :: MonadInterpreter m => Object -> m Int
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

newClass :: (MonadIO m) => Text -> [ClassInfo] -> [(Text, ObjectRef)] -> m Object
newClass name bases dict = do
    ref <- newRef $ HashMap.fromList dict

    return . Class $ ClassInfo {
        className = name,
        classBases = bases,
        classDict = ref
    }

newDict :: MonadInterpreter m => [(Object, Object)] -> m Object
newDict items = do
    dict <- new "dict" []
    forM_ items $ \(key, value) ->
        invoke dict "__setitem__" [key, value]
    return dict

newFloat :: MonadInterpreter m => Double -> m Object
newFloat d = return $ Float d

newFunction :: MonadInterpreter m => Text -> [FnParam] -> [Statement] -> m Object
newFunction name params block = return $ Function name params block

newImag :: MonadInterpreter m => Complex Double -> m Object
newImag i = return $ Imaginary i

newInt :: MonadInterpreter m => Integer -> m Object
newInt i = return $ Int i

newList :: (MonadInterpreter m, MonadIO m) => [Object] -> m Object
newList items = do
    list <- new "list" []
    mapM_ (\item -> invoke list "append" [item]) items
    return list

newObject :: (MonadIO m) => ClassInfo -> m Object
newObject cls = do
    clsDict <- readRef . classDict $ cls
    dict <- newRef clsDict
    info <- pure ObjectInfo {
        objectClass = cls,
        objectDict = dict
    }

    return $ Object info

newSet :: MonadInterpreter m => [Object] -> m Object
newSet items = do
    set <- new "set" []
    mapM_ (\item -> invoke set "add" [item]) items
    return set

newString :: MonadInterpreter m => Text -> m Object
newString s = return $ String s

newTuple :: MonadInterpreter m => [Object] -> m Object
newTuple l = return $ Tuple l

isNone :: Object -> Bool
isNone (None) = True
isNone _ = False

equal :: MonadInterpreter m => Object -> Object -> m Bool
equal (Bool l) (Bool r)             = return $ l == r
equal (Bytes l) (Bytes r)           = return $ l == r
equal (Float l) (Float r)           = return $ l == r
equal (Imaginary l) (Imaginary r)   = return $ l == r
equal (Int l) (Int r)               = return $ l == r
equal (String l) (String r)         = return $ l == r
equal (BuiltinFn l) (BuiltinFn r)   = return $ l == r
equal (Tuple l) (Tuple r)           =
    if length l /= length r
        then return False
        else do
            results <- zipWithM equal l r
            return $ all (== True) results
equal (List l) (List r) = do
    left    <- readRef l
    right   <- readRef r
    equal (Tuple left) (Tuple right)
equal (Set l) (Set r) = do
    left    <- readRef l
    right   <- readRef r
    equal (Tuple $ IntMap.elems left) (Tuple $ IntMap.elems right)
equal (Dict l) (Dict r) = do
    left    <- readRef l
    right   <- readRef r
    if IntMap.size left /= IntMap.size right
        then return False
        else do
            results <- zipWithM pairEqual (IntMap.elems left) (IntMap.elems right)
            return $ all (== True) results
  where
    pairEqual (lk, lv) (rk, rv) = do
        k <- equal lk rk
        v <- equal lv rv
        return $ k && v
equal _ _                           = return False

isTruthy :: MonadInterpreter m => Object -> m Bool
isTruthy (None) = return False
isTruthy (Bool False) = return False
isTruthy (Int 0) = return False
isTruthy (Float 0.0) = return False
isTruthy (String s) = return . not . T.null $ s
isTruthy (Bytes b) = return $ not (B.null b)
isTruthy (List ref) = do
    l <- readRef ref
    return $ not (null l)
isTruthy obj@(Object {}) = isTruthy =<< invoke obj "__bool__" []
isTruthy (Tuple objs) = return $ not $ null objs
isTruthy _ = return True

toStr :: MonadInterpreter m => Object -> m String
toStr (None) = return "None"
toStr (Bool b) = return $ if b then "True" else "False"
toStr (Bytes _b) = return "b'??'"
toStr (Float f) = return $ show f
toStr (Function name _ _) = return . T.unpack $ name
toStr (Imaginary i)
    | realPart i == 0   = return $ show i
    | otherwise         = return $ show i
toStr (Int i) = return $ show i
toStr (String s) = return $ T.unpack s
toStr (List ref) = do
    l <- readRef ref
    strItems <- mapM toStr l
    return $ "[" ++ intercalate ", " strItems ++ "]"
toStr (Tuple objs) = do
    strItems <- mapM toStr objs
    case strItems of
        [str]   -> return $ "(" ++ str ++ ",)"
        _       -> return $ "(" ++ intercalate ", " strItems ++ ")"
toStr (Set ref) = do
    items <- readRef ref
    strItems <- mapM toStr $ IntMap.elems items
    return $ "{" ++ intercalate ", " strItems ++ "}"
toStr (Dict ref) = do
    items <- readRef ref
    strItems <- forM (IntMap.elems items) $ \(k, v) -> do
        key     <- toStr k
        value   <- toStr v
        return $ key ++ ": " ++ value
    return $ "{" ++ intercalate ", " strItems ++ "}"
toStr (BuiltinFn name)  = return $ "<built-in function " ++ T.unpack name ++ ">"
toStr (Class info) = return $ "<class '" ++ T.unpack (className info) ++ "'>"
toStr obj@(Object {}) = do
    str <- invoke obj "__str__" []
    case str of
        String s    -> return . T.unpack $ s
        _           -> toStr str
toStr (Method name (ClassBinding clsName _) _ _) = 
    return $ "<method '" ++ T.unpack name ++ "' of '" ++ T.unpack clsName ++ "' objects>"
toStr (Method name (InstanceBinding clsName obj) _ _) = do
    s <- toStr obj
    return $ "<bound method " ++ T.unpack clsName ++ "." ++ T.unpack name ++ " of " ++ s ++ ">"
