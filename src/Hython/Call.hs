module Hython.Call (call)
where

import Control.Monad (zipWithM_)
import Control.Monad.Cont (callCC)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (pack)

import Hython.Builtins (callBuiltin)
import Hython.Object

import Language.Python

call :: (MonadCont m, MonadInterpreter m, MonadIO m) => Object -> [Object] -> m Object
call (BuiltinFn name) args = callBuiltin name args

call (Function name params statements) args = do
    result <- callCC $ \returnCont -> do
        pushEnvFrame
        zipWithM_ bindArg params args
        pushControlCont ReturnCont returnCont

        evalBlock statements
        return None

    popEnvFrame
    popControlCont ReturnCont
    return result
  where
    bindArg (FormalParam name) obj = bind (pack name) obj


call _ _ = do
    raise "TypeError" "object is not callable"
    return None


