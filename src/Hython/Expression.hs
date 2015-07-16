module Hython.Expression (evalExpr, isTruthy) where

import qualified Data.ByteString as B
import Data.Text

import Language.Python

import Hython.Builtins (callBuiltin)
import Hython.Monad
import Hython.Object

evalExpr :: MonadInterpreter m => Expression -> m Object
evalExpr (Constant c) = case c of
    ConstantNone        -> newNone
    ConstantBool b      -> newBool b
    ConstantBytes b     -> newBytes b
    ConstantFloat f     -> newFloat f
    ConstantImag i      -> newImag i
    ConstantInt i       -> newInt i
    ConstantString s    -> newString s

evalExpr (Name name) = do
    result <- lookupName (pack name)
    case result of
        Just obj    -> return obj
        Nothing     -> do
            raiseError "NameError" "name not defined"
            return None

evalExpr (Call expr argExprs) = do
    callable <- evalExpr expr
    args <- mapM evalExpr argExprs

    case callable of
        (BuiltinFn name)    -> do
            result <- callBuiltin name args
            case result of
                Right obj   -> return obj
                Left msg    -> do
                    raiseError "NameError" msg
                    return None

        _                   -> do
            raiseError "TypeError" "object is not callable"
            return None

isTruthy :: Object -> Bool
isTruthy (None) = False
isTruthy (Bool False) = False
isTruthy (Int 0) = False
isTruthy (Float 0.0) = False
isTruthy (String "") = False
isTruthy (Bytes b) = not (B.null b)
isTruthy _ = True
