module Hython.Class
where

import Data.IORef
import qualified Data.HashMap.Strict as Map

import qualified Hython.AttributeDict as AttributeDict
import Hython.Object

classOf :: Object -> Object
classOf (Object c@(ClassObj {}) _) = c
classOf _ = error "classOf requires an object to be passed"

isSubClass :: Object -> Object -> Bool
isSubClass (ClassObj derivedName bases _) (ClassObj baseName _ _) = derivedName == baseName || any isBase bases
  where
    isBase (ClassObj base _ _) = baseName == base
    isBase _ = error "isSubClass: expected class in bases list"
isSubClass _ _ = error "isSubClass() arg 1 must be a class"

newClass :: String -> Objects -> IO Object
newClass name bases = do
    dict <- newIORef Map.empty
    return $ ClassObj name bases dict

newObject :: Object -> IO Object
newObject cls@(ClassObj _ _ classAttributes) = do
    instanceDict <- AttributeDict.clone classAttributes
    AttributeDict.update "__class__" cls instanceDict

    return $ Object cls instanceDict

newObject _ = error "newObject: arg 1 must be a class"
