module Hython.Types
where

import Prelude hiding (mod)

import Control.Monad (forM_, zipWithM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.Hashable as H
import qualified Data.ByteString.Char8 as B
import Data.Complex (Complex, realPart, imagPart)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

import Language.Python (Statement)

import qualified Hython.AttributeDict as AD
import Hython.ControlFlow (MonadFlow)
import Hython.Environment (Environment)
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
            | BuiltinFn Text
            | Function Text [FnParam] [Statement] Env
            | Method Text MethodBinding [FnParam] [Statement] Env
            | Class ClassInfo
            | Object ObjectInfo
            | Lambda [FnParam] Statement Env
            | Module ModuleInfo

type ObjectRef = Ref Object

data MethodBinding = ClassBinding Text Object
                   | InstanceBinding Text Object

data FnParam = NamedParam Text
             | DefParam Text Object
             | SParam Text
             | DSParam Text

data ClassInfo = ClassInfo
               { classId        :: UUID
               , className      :: Text
               , classModule    :: ModuleInfo
               , classBases     :: [ClassInfo]
               , classDict      :: Ref AttributeDict
               }

instance Eq ClassInfo where
    l == r = className l == className r

data ObjectInfo = ObjectInfo
                { objectId      :: UUID
                , objectClass   :: ClassInfo
                , objectDict    :: Ref AttributeDict
                }

data ModuleInfo = ModuleInfo
                { moduleId      :: UUID
                , moduleName    :: Text
                , modulePath    :: FilePath
                , moduleDict    :: Ref AttributeDict
                }

type DictRef    = Ref (IntMap (Object, Object))
type ListRef    = Ref [Object]
type SetRef     = Ref (IntMap Object)

type AttributeDict = AD.AttributeDict Object

type Env = Environment Object

data ExceptionHandler = ExceptionHandler Name ClassInfo [Statement]

instance Eq ModuleInfo where
    l == r = modulePath l == modulePath r

class HasAttributes a where
    getObjAttrs :: a -> Maybe (Ref AttributeDict)

instance HasAttributes Object where
    getObjAttrs (Object info)   = Just $ objectDict info
    getObjAttrs (Class info)    = Just $ classDict info
    getObjAttrs (Module info)   = Just $ moduleDict info
    getObjAttrs _ = Nothing

class (MonadFlow Object (Object -> m ()) m, MonadIO m) => MonadInterpreter m where
    evalBlock           :: [Statement] -> m ()
    invoke              :: Object -> String -> [Object] -> m Object
    new                 :: String -> [Object] -> m Object
    pushEvalResult      :: String -> m ()
    raise               :: String -> String -> m ()
    getCurrentModule    :: m ModuleInfo
    getModuleByPath     :: FilePath -> m (Maybe ModuleInfo)
    setCurrentModule    :: ModuleInfo -> m ()

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
    (Object {}) -> do
        result <- invoke obj "__hash__" []
        case result of
            (Int i) -> return $ fromInteger i
            _       -> do
                raise "SystemError" "non-int returned from __hash__"
                return 0
    _               -> do
        raise "TypeError" "unhashable type"
        return 0

newNone :: MonadInterpreter m => m Object
newNone = return None

newBool :: MonadInterpreter m => Bool -> m Object
newBool b = return $ Bool b

newBytes :: MonadInterpreter m => String -> m Object
newBytes b = return $ Bytes (B.pack b)

newClass :: (MonadIO m) => Text -> [ClassInfo] -> [(Text, ObjectRef)] -> ModuleInfo -> m Object
newClass name bases dict mod = do
    ref     <- newRef $ AD.fromList dict
    clsid   <- liftIO nextRandom

    return . Class $ ClassInfo {
        classId = clsid,
        className = name,
        classBases = bases,
        classDict = ref,
        classModule = mod
    }

newDict :: MonadInterpreter m => [(Object, Object)] -> m Object
newDict items = do
    dict <- new "dict" []
    forM_ items $ \(key, value) ->
        invoke dict "__setitem__" [key, value]
    return dict

newFloat :: MonadInterpreter m => Double -> m Object
newFloat d = return $ Float d

newFunction :: MonadInterpreter m => Text -> [FnParam] -> [Statement] -> Env -> m Object
newFunction name params block env = return $ Function name params block env

newImag :: MonadInterpreter m => Complex Double -> m Object
newImag i = return $ Imaginary i

newInt :: MonadInterpreter m => Integer -> m Object
newInt i = return $ Int i

newLambda :: Monad m => [FnParam] -> Statement -> Env -> m Object
newLambda params statement env = return $ Lambda params statement env

newList :: (MonadInterpreter m, MonadIO m) => [Object] -> m Object
newList items = do
    list <- new "list" []
    mapM_ (\item -> invoke list "append" [item]) items
    return list

newModule :: MonadIO m => Text -> FilePath -> m Object
newModule name path = do
    ref     <- newRef AD.empty
    modid   <- liftIO nextRandom

    return . Module $ ModuleInfo
        { moduleId = modid
        , moduleName = name
        , modulePath = path
        , moduleDict = ref
        }

newObject :: (MonadIO m) => ClassInfo -> m Object
newObject cls = do
    clsDict <- readRef . classDict $ cls
    dict    <- newRef clsDict
    objid   <- liftIO nextRandom

    return . Object $ ObjectInfo
        { objectId = objid
        , objectClass = cls
        , objectDict = dict
        }

newSet :: MonadInterpreter m => [Object] -> m Object
newSet items = do
    set <- new "set" []
    mapM_ (\item -> invoke set "add" [item]) items
    return set

newString :: MonadInterpreter m => Text -> m Object
newString s = return $ String s

newTuple :: MonadInterpreter m => [Object] -> m Object
newTuple items = do
    args    <- newList items
    new "tuple" [args]

isNone :: Object -> Bool
isNone (None) = True
isNone _ = False

equal :: MonadInterpreter m => Object -> Object -> m Bool
equal lhs rhs = case (lhs, rhs) of
    -- conversion
    (Bool l, _) -> equal (Float (boolToFloat  l)) rhs
    (Int  l, _) -> equal (Float (fromIntegral l)) rhs
    (_, Bool r) -> equal lhs (Float (boolToFloat  r))
    (_, Int  r) -> equal lhs (Float (fromIntegral r))
    (None,        None       ) -> return True
    (Bytes     l, Bytes     r) -> return $ l == r
    (Float     l, Float     r) -> return $ l == r
    (Imaginary l, Imaginary r) -> return $ l == r
    (String    l, String    r) -> return $ l == r
    (BuiltinFn l, BuiltinFn r) -> return $ l == r
    (Dict      l, Dict      r) -> do
        left    <- readRef l
        right   <- readRef r
        if IntMap.size left /= IntMap.size right
            then return False
            else do
                results <- zipWithM pairEqual (IntMap.elems left) (IntMap.elems right)
                return $ all (== True) results
    (_, _) -> return False
  where
    boolToFloat False = 0.0
    boolToFloat True  = 1.0
    pairEqual (lk, lv) (rk, rv) = do
        k <- equal lk rk
        v <- equal lv rv
        return $ k && v

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
isTruthy _ = return True

toStr :: MonadInterpreter m => Object -> m String
toStr (None) = return "None"
toStr (Bool b) = return $ if b then "True" else "False"
toStr (Bytes _b) = return "b'??'"
toStr (Float f) = return $ show f
toStr (Function name _ _ _) = return . T.unpack $ name
toStr (Imaginary i)
    | realPart i == 0   = return $ show (imagPart i) ++ "j"
    | otherwise         = return $ show i
toStr (Int i) = return $ show i
toStr (String s) = return $ T.unpack s
toStr (Set ref) = do
    items <- readRef ref
    strItems <- mapM toStr $ IntMap.elems items
    return $ "{" ++ intercalate ", " strItems ++ "}"
toStr (BuiltinFn name)  = return $ "<built-in function " ++ T.unpack name ++ ">"
toStr (List _) = return "<internal listref>"
toStr (Dict _) = return "<internal dictref>"
toStr (Class info) = return $ "<class '" ++ T.unpack (moduleName . classModule $ info) ++ "." ++ T.unpack (className info) ++ "'>"
toStr (Lambda {}) = return "<function <lambda>>"
toStr obj@(Object {}) = do
    str <- invoke obj "__str__" []
    case str of
        String s    -> return . T.unpack $ s
        _           -> toStr str
toStr (Method name (ClassBinding clsName _) _ _ _) = 
    return $ "<method '" ++ T.unpack name ++ "' of '" ++ T.unpack clsName ++ "' objects>"
toStr (Method name (InstanceBinding clsName obj) _ _ _) = do
    s <- toStr obj
    return $ "<bound method " ++ T.unpack clsName ++ "." ++ T.unpack name ++ " of " ++ s ++ ">"
toStr (Module info) = return $ "<module '" ++ T.unpack (moduleName info) ++ "'"
