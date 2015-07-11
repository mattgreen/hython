module Hython.Expression
where

import Data.Text

import Language.Python

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
        Nothing     -> error "NameError: name not defined"
