module Hython.Object
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Complex (Complex)
import Data.IORef (IORef, newIORef, readIORef)

import Language.Python (Statement)

import Hython.Name

data Object = None
            | Bool Bool
            | Bytes ByteString
            | Float Double
            | Imaginary (Complex Double)
            | Int Integer
            | String String
            | List (IORef [Object])
            | Tuple (IORef [Object])
            | BuiltinFn String

type ObjectRef = IORef Object

class Monad m => MonadEnvironment m where
    bind            :: Name -> Object -> m ()
    bindGlobal      :: Name -> m ()
    bindNonlocal    :: Name -> m ()
    lookupName      :: Name -> m (Maybe Object)
    unbind          :: Name -> m ()

class MonadEnvironment m => MonadInterpreter m where
    evalBlock   :: [Statement] -> m [Object]
    raise       :: String -> String -> m ()

newNone :: MonadInterpreter m => m Object
newNone = return None

newBool :: MonadInterpreter m => Bool -> m Object
newBool b = return $ Bool b

newBytes :: MonadInterpreter m => String -> m Object
newBytes b = return $ Bytes (B.pack b)

newFloat :: MonadInterpreter m => Double -> m Object
newFloat d = return $ Float d

newImag :: MonadInterpreter m => Complex Double -> m Object
newImag i = return $ Imaginary i

newInt :: MonadInterpreter m => Integer -> m Object
newInt i = return $ Int i

newList :: (MonadInterpreter m, MonadIO m) => [Object] -> m Object
newList l = do
    ref <- liftIO $ newIORef l
    return $ List ref

newString :: MonadInterpreter m => String -> m Object
newString s = return $ String s

newTuple :: (MonadInterpreter m, MonadIO m) => [Object] -> m Object
newTuple l = do
    ref <- liftIO $ newIORef l
    return $ Tuple ref

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
isTruthy (Tuple ref) = do
    l <- liftIO $ readIORef ref
    return $ not (null l)
isTruthy _ = return True
