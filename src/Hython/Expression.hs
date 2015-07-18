module Hython.Expression (evalExpr, isTruthy) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as B
import Data.Bits (complement)
import Data.Text

import Language.Python

import Hython.Builtins (callBuiltin)
import Hython.Object

evalExpr :: (MonadIO m, MonadInterpreter m) => Expression -> m Object
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
                Just obj    -> return obj
                Nothing     -> do
                    raiseError "NameError" "built-in not found"
                    return None

        _                   -> do
            raiseError "TypeError" "object is not callable"
            return None

evalExpr (UnaryOp op expr) = do
    obj <- evalExpr expr
    case (op, obj) of
        (Not, Bool b)       -> newBool (not b)
        (Pos, Int i)        -> newInt i
        (Neg, Int i)        -> newInt (-i)
        (Pos, Float f)      -> newFloat f
        (Neg, Float f)      -> newFloat (-f)
        (Complement, Int i) -> newInt (complement i)
        _                   -> do
            raiseError "SystemError" ("Unsupported operand type: " ++ show op)
            return None

isTruthy :: Object -> Bool
isTruthy (None) = False
isTruthy (Bool False) = False
isTruthy (Int 0) = False
isTruthy (Float 0.0) = False
isTruthy (String "") = False
isTruthy (Bytes b) = not (B.null b)
isTruthy _ = True
