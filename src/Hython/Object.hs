{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hython.Object
where

import Data.Complex
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.IORef

import Language.Python

data Object
    = String String
    | Int Integer
    | Float Double
    | Imaginary (Complex Double)
    | Bool Bool
    | BuiltinFn String
    | Lambda [Param] Expression Env
    | Function String [Param] [Statement] Env
    | ModuleObj Module
    | ClassObj Class
    | Object Class AttributeDict
    | Slice Object Object Object
    | Tuple Objects
    | List (IORef Objects)
    | Dict (IORef DictData)
    | None
    deriving(Eq, Show)

type AttributeDict = IORef (HashMap String (IORef Object))
type DictData = HashMap Int (IORef Object)

data Class = Class
    { className         :: String
    , classBases        :: [Class]
    , classModule       :: Module
    , classDict         :: AttributeDict
    } deriving (Eq, Show)

data Module = Module
    { moduleName        :: String
    , modulePath        :: String
    , moduleDict        :: AttributeDict
    } deriving (Eq, Show)

type Objects = [Object]

data Env = Env
    { localEnv          :: AttributeDict
    , enclosingEnvs     :: [AttributeDict]
    , moduleEnv         :: AttributeDict
    , builtinEnv        :: AttributeDict
    , activeEnv         :: ActiveEnv
    } deriving (Eq, Show)

data ActiveEnv
    = ModuleEnv
    | LocalEnv
    deriving (Eq, Show)

instance Hashable Object where
    hashWithSalt s (String o)     = s + hash o
    hashWithSalt s (Int n)        = s + hash n
    hashWithSalt s (Float n)      = s + hash n
    hashWithSalt s (Imaginary n)  = s + hash (realPart n) + hash (imagPart n)
    hashWithSalt s (Bool b)         = s + hash b
    hashWithSalt s (BuiltinFn str)  = s + hash str
    hashWithSalt s (Lambda p st _)   = s + hash (show p) + hash (show st)
    hashWithSalt s (Function n p st _)   = s + hash n + hash (show p) + hash (show st)

instance Show (IORef a) where
    show _ = "<ioref>"

class Truthy a where
    isTrue :: a -> Bool

instance Truthy Object where
    isTrue (Int 0)      = False
    isTrue (Bool False) = False
    isTrue (None)       = False
    isTrue _            = True
