module Hython.Scoping
where

import Control.Monad
import qualified Data.HashMap.Strict as Map

import Language.Python.Core
import Hython.Environment

pushEnclosingScope :: [(String, Value)] -> Scope -> Scope
pushEnclosingScope symbols scope =
    scope { enclosingScopes = Map.fromList symbols : enclosingScopes scope }

popEnclosingScope :: Scope -> Scope
popEnclosingScope scope = scope { enclosingScopes = remainingScopes (enclosingScopes scope) }
  where
    remainingScopes [] = []
    remainingScopes (_:ss) = ss

bindName :: String -> Value -> Scope -> Scope
bindName name object scope = case enclosingScopes scope of
    []      -> scope { globalScope = bind (globalScope scope) }
    (s:ss)  -> scope { enclosingScopes = bind s : ss }
  where
    bind = Map.insert name object

lookupName :: String -> Scope -> Maybe Value
lookupName name scope = do
    let searchScopes = enclosingScopes scope ++ [globalScope scope] ++ [builtinScope scope]
    foldr (mplus . Map.lookup name) Nothing searchScopes

unbindName :: String -> Scope -> Scope
unbindName name scope = case enclosingScopes scope of
    []      -> scope { globalScope = unbind (globalScope scope) }
    (s:ss)  -> scope { enclosingScopes = unbind s : ss }
  where
    unbind = Map.delete name

