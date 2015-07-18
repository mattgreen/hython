module Hython.Object
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Complex (Complex)
import Data.IORef (IORef)

import Language.Python (Statement)

import Hython.Name

data Object = None
            | Bool Bool
            | Bytes ByteString
            | Float Double
            | Imaginary (Complex Double)
            | Int Integer
            | String String
            | BuiltinFn String
            deriving (Eq)

type ObjectRef = IORef Object

class Monad m => MonadInterpreter m where
    bind            :: Name -> Object -> m ()
    bindGlobal      :: Name -> m ()
    bindNonlocal    :: Name -> m ()
    lookupName      :: Name -> m (Maybe Object)
    unbind          :: Name -> m ()

    evalBlock       :: [Statement] -> m [Object]

    raiseError      :: String -> String -> m ()

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

newString :: MonadInterpreter m => String -> m Object
newString s = return $ String s
