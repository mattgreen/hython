module Hython.NameResolution
where

import Control.Monad
import qualified Data.HashMap.Strict as Map

import Language.Python.Core
import Hython.Environment

pushEnclosingScope :: [(String, Object)] -> Scope -> Scope
pushEnclosingScope symbols scope =
    scope { enclosingScopes = Map.fromList symbols : enclosingScopes scope }

popEnclosingScope :: Scope -> Scope
popEnclosingScope scope =
    scope { enclosingScopes = remainingScopes (enclosingScopes scope) }
  where
    remainingScopes [] = []
    remainingScopes (_:ss) = ss

bindName :: String -> Object -> Scope -> Scope
bindName name object scope = case enclosingScopes scope of
    []      -> scope { globalScope = bind $ globalScope scope }
    (s:ss)  -> scope { enclosingScopes = bind s : ss }
  where
    bind = Map.insert name object

lookupName :: String -> Scope -> Maybe Object
lookupName name scope = do
    let currentScopes = enclosingScopes scope

    lookupIn $ currentScopes ++ [globalScope scope, builtinScope scope]
  where
    lookupIn scopes = foldr (mplus . Map.lookup name) Nothing scopes

unbindName :: String -> Scope -> Scope
unbindName name scope = case enclosingScopes scope of
    []      -> scope { globalScope = unbind (globalScope scope) }
    (s:ss)  -> scope { enclosingScopes = unbind s : ss }
  where
    unbind = Map.delete name

