module Hython.Statement (eval)
where

import Language.Python

import Hython.Expression
import Hython.Monad
import Hython.Object

eval :: MonadInterpreter m => Statement -> m (Maybe Object)
eval (Expression e) = do
    obj <- evalExpr e
    return $ Just obj

eval _ = error "statement not implemented"
