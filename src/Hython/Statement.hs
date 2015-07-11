module Hython.Statement (eval)
where

import Data.Text

import Language.Python

import Hython.Expression
import Hython.Monad
import Hython.Object

eval :: MonadInterpreter m => Statement -> m (Maybe Object)
eval (Assignment (Name name) expr) = do
    value <- evalExpr expr
    bind (pack name) value
    return Nothing

eval (Expression e) = do
    obj <- evalExpr e
    return $ Just obj

eval _ = error "statement not implemented"
