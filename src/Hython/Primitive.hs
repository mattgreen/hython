module Hython.Primitive (callPrimitive)
where

import Hython.BuiltinTypes.Dict
import Hython.Types

callPrimitive :: MonadInterpreter m => String -> [Object] -> m Object
callPrimitive prim args = case (prim, args) of
    ("dict-clear", [Dict ref])          -> dictClear ref
    ("dict-contains", [Dict ref, obj])  -> dictContains ref obj
    ("dict-del", [Dict ref, obj])       -> dictDel ref obj
    ("dict-get", [Dict ref, obj])       -> dictGet ref obj
    ("dict-length", [Dict ref])         -> dictLength ref
    ("dict-new", [])                    -> dictNew
    ("dict-set", [Dict ref, key, obj])  -> dictSet ref key obj
    _ -> do
        raise "SystemError" ("invalid primitive: " ++ prim)
        return None
