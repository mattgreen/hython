module Hython.Expression
where

import Language.Python

import Hython.Monad
import Hython.Object

evalExpr :: MonadInterpreter m => Expression -> m Object
evalExpr (Constant c) = return $ case c of
    ConstantInt v   -> Object
