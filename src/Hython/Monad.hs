module Hython.Monad
where

import Language.Python

import Hython.Name
import Hython.Object

class Monad m => MonadInterpreter m where
    bind            :: Name -> Object -> m ()
    bindGlobal      :: Name -> m ()
    bindNonlocal    :: Name -> m ()
    lookupName      :: Name -> m (Maybe Object)
    unbind          :: Name -> m ()

    evalBlock       :: [Statement] -> m [Object]

