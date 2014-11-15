module Hython.Classes
where

import Data.IORef
import qualified Data.HashMap.Strict as Map

import Hython.Attributes
import Language.Python.Core

classOf :: Value -> Value
classOf (Object c@(Class {}) _) = c
classOf _ = error "classOf requires an object to be passed"

isSubClass :: Value -> Value -> Bool
isSubClass (Class derivedName bases _) (Class baseName _ _) = derivedName == baseName || any isBase bases
  where
    isBase (Class base _ _) = baseName == base
isSubClass _ _ = error "isSubClass() arg 1 must be a class"

newClass :: String -> Values -> IO Value
newClass name bases = do
    dict <- newIORef Map.empty
    return $ Class name bases dict

newObject :: Value -> IO Value
newObject cls@(Class _ _ classAttributes) = do
    instanceDict <- cloneAttributeDict classAttributes
    writeAttr "__class__" cls instanceDict

    return $ Object cls instanceDict

