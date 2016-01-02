{-# LANGUAGE OverloadedStrings #-}

module Hython.Call (call, invoke, new)
where

import Control.Monad (forM, when, zipWithM)
import Control.Monad.Cont (callCC)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Safe (atDef)

import Hython.Builtins (callBuiltin, getAttr, setAttr)
import qualified Hython.ControlFlow as ControlFlow
import Hython.Environment (lookupName, pushEnvFrame, popEnvFrame)
import Hython.Types hiding (invoke, new)

call :: (MonadCont m, MonadInterpreter m, MonadIO m) => Object -> [Object] -> [(Text, Object)] -> m Object
call (BuiltinFn name) args _ = callBuiltin name args

call cls@(Class info) args kwargs = do
    obj <- newObject info
    setAttr "__class__" cls obj

    mconstructor <- getAttr "__init__" obj
    _ <- case mconstructor of
        Just ctor   -> call ctor args kwargs
        Nothing     -> return None

    return obj

call (Function fnName params statements) args kwargs = do
    requiredParams <- pure $ takeWhile isRequiredParam params
    when (length args < length requiredParams) $
        raise "TypeError" ("not enough arguments passed to '" ++ show fnName ++ "'")
    when (length requiredParams == length params && length args > length requiredParams) $
        raise "TypeError" ("too many args passed to '" ++ show fnName ++ "'")

    result <- callCC $ \returnCont -> do
        bindings <- zipWithM getArg params [0..]
        pushEnvFrame bindings

        ControlFlow.pushFrame returnCont

        evalBlock statements
        return None

    _ <- popEnvFrame
    ControlFlow.popFrame

    return result

  where
    getArg (NamedParam name) i = return (name, args !! i)
    getArg (DefParam name obj) i = return (name, atDef obj args i)
    getArg (SParam name) i = do
        tuple <- newTuple (drop i args)
        return (name, tuple)
    getArg (DSParam name) _ = do
        items <- forM kwargs $ \(k,v) -> do
            s <- newString k
            return (s, v)
        dict <- newDict items
        return (name, dict)

    isRequiredParam (NamedParam _) = True
    isRequiredParam _ = False

call (Method name receiver params statements) args kwargs =
    case receiver of
        ClassBinding _ cls      -> call (Function name params statements) (cls:args) kwargs
        InstanceBinding _ obj   -> call (Function name params statements) (obj:args) kwargs

call _ _ _ = do
    raise "TypeError" "object is not callable"
    return None

invoke :: (MonadCont m, MonadInterpreter m) => Object -> String -> [Object] -> [(Text, Object)] -> m Object
invoke target methodName args kwargs = do
    mmethod <- getAttr (T.pack methodName) target

    case mmethod of
        Just method -> call method args kwargs
        Nothing     -> do
            raise "AttributeError" ("object has no attribute '" ++ methodName ++ "'")
            return None

new :: (MonadCont m, MonadInterpreter m) => String -> [Object] -> m Object
new clsName args = do
    mcls <- lookupName $ T.pack clsName
    case mcls of
        Just cls    -> call cls args []
        Nothing     -> do
            raise "SystemError" (clsName ++ " not found!")
            return None
