module Hython.Environment
    ( Environment
    , bind
    , bindGlobal
    , bindNonlocal
    , lookup
    , new
    , push
    , pop
    , unbind
    ) where

import Prelude hiding (lookup)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Text (unpack)
import Safe (headDef)
import Hython.Name

data Environment a = Environment
    { envModule     :: HashMap Name (Binding a)
    , envBuiltins   :: HashMap Name (Binding a)
    , envFrames     :: [HashMap Name (Binding a)]
    }
    deriving (Show)

data Binding a
    = LocalBinding a
    | NonlocalBinding
    | GlobalBinding
    deriving (Show)

bind :: Name -> a -> Environment a -> Environment a
bind name obj env = case envFrames env of
    (e:es)  -> case Map.lookup name e of
        Just GlobalBinding    -> env { envModule = Map.insert name (LocalBinding obj) (envModule env) }
        Just NonlocalBinding    -> do
            let (before, x:xs) = break (Map.member name) es
            env { envFrames = before ++ [Map.insert name (LocalBinding obj) x] ++ xs }
        _                   -> env { envFrames = Map.insert name (LocalBinding obj) e : es }
    []      -> env { envModule = Map.insert name (LocalBinding obj) (envModule env) }

bindGlobal :: Name -> Environment a -> Environment a
bindGlobal name env = case envFrames env of
    (e:es)  -> env { envFrames = Map.insert name GlobalBinding e : es }
    []      -> env

bindNonlocal :: Name -> Environment a -> Either String (Environment a)
bindNonlocal name env = case envFrames env of
    (e:es)  -> if any (Map.member name) es
                   then Right $ env { envFrames = Map.insert name NonlocalBinding e : es }
                   else Left $ "no binding for nonlocal '" ++ unpack name ++ "' found"
    []      -> Left "nonlocal declaration not allowed at module level"

lookup :: Name -> Environment a -> Maybe a
lookup name env = lookupIn $ [headDef Map.empty (envFrames env)] ++ [envModule env] ++ [envBuiltins env]
  where
    lookupIn (e:es) = case Map.lookup name e of
        Just objRef -> case objRef of
            LocalBinding obj    -> Just obj
            _                   -> lookupIn es
        Nothing     -> lookupIn es
    lookupIn _     = Nothing

new :: [(Name, a)] -> Environment a
new builtins = Environment { envBuiltins = Map.fromList refs, envFrames = [], envModule = Map.empty }
  where
    refs = map (\x -> (fst x, LocalBinding (snd x))) builtins

push :: Environment a -> Environment a
push env = env { envFrames = Map.empty : envFrames env }

pop :: Environment a -> Environment a
pop env = case envFrames env of
    _:es    -> env { envFrames = es }
    []      -> env

unbind :: Name -> Environment a -> Either String (Environment a)
unbind name env = if any (Map.member name) searchedFrames
    then Right $ case envFrames env of
        (e:es)  -> env { envFrames = Map.delete name e : es }
        []      -> env { envModule = Map.delete name (envModule env) }
    else Left $ "name '" ++ unpack name ++ "' is not defined"
  where
    searchedFrames = envFrames env ++ [envModule env]
