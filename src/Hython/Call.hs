module Hython.Call (call)
where

import Control.Monad (forM_, when, zipWithM)
import Control.Monad.Cont (callCC)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (pack)
import Safe (atDef)

import Hython.Builtins (callBuiltin)
import Hython.Object

call :: (MonadCont m, MonadInterpreter m, MonadIO m) => Object -> [Object] -> m Object
call (BuiltinFn name) args = callBuiltin name args

call (Function fnName params statements) args = do
    requiredParams <- pure $ takeWhile isRequiredParam params
    when (length args < length requiredParams) $
        raise "TypeError" ("not enough arguments passed to '" ++ fnName ++ "'")
    when (length requiredParams == length params && length args > length requiredParams) $
        raise "TypeError" ("too many args passed to '" ++ fnName ++ "'")

    result <- callCC $ \returnCont -> do
        pushEnvFrame
        bindings <- zipWithM getArg params [0..]
        forM_ bindings $ \(name, obj) ->
            bind (pack name) obj

        pushControlCont ReturnCont returnCont

        evalBlock statements
        return None

    popEnvFrame
    popControlCont ReturnCont

    return result

  where
    getArg (NamedParam name) i     = return (name, args !! i)
    getArg (DefParam name obj) i   = return (name, atDef obj args i)
    getArg (SParam name) i         = do
        tuple <- newTuple (drop i args)
        return (name, tuple)

    isRequiredParam (NamedParam _) = True
    isRequiredParam _ = False

call _ _ = do
    raise "TypeError" "object is not callable"
    return None


