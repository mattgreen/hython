module Hython.Classes
where

import Data.IORef
import qualified Data.HashMap.Strict as Map

import qualified Hython.AttributeDict as AttributeDict
import Language.Python.Core

classOf :: Object -> Object
classOf (Object c@(Class {}) _) = c
classOf _ = error "classOf requires an object to be passed"

isSubClass :: Object -> Object -> Bool
isSubClass (Class derivedName bases _) (Class baseName _ _) = derivedName == baseName || any isBase bases
  where
    isBase (Class base _ _) = baseName == base
isSubClass _ _ = error "isSubClass() arg 1 must be a class"

newClass :: String -> Objects -> IO Object
newClass name bases = do
    dict <- newIORef Map.empty
    return $ Class name bases dict

newObject :: Object -> IO Object
newObject cls@(Class _ _ classAttributes) = do
    instanceDict <- AttributeDict.clone classAttributes
    AttributeDict.update "__class__" cls instanceDict

    return $ Object cls instanceDict

