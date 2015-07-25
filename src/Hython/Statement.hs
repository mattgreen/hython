module Hython.Statement (eval)
where

import Control.Monad.IO.Class (MonadIO)
import Data.Text

import Language.Python

import Hython.Expression
import Hython.Object

eval :: (MonadIO m, MonadInterpreter m) => Statement -> m Object
eval (Assignment (Name name) expr) = do
    value <- evalExpr expr
    bind (pack name) value
    return None

eval (Del (Name name)) = do
    unbind (pack name)
    return None

eval (Del _) = do
    raise "SystemError" "invalid del statement"
    return None

eval (Expression e) = evalExpr e

eval (Global names) = do
    mapM_ (bindGlobal . pack) names
    return None

eval (If clauses elseBlock) = case clauses of
    [] -> do
        _ <- evalBlock elseBlock
        return None
    (IfClause condition block : rest) -> do
        result  <- evalExpr condition
        truthy  <- isTruthy result
        if truthy
            then do
                _ <- evalBlock block
                return None
            else eval (If rest elseBlock)

eval (Nonlocal names) = do
    mapM_ (bindNonlocal . pack) names
    return None

eval (Pass) = return None

eval _ = do
    raise "SystemError" "statement not implemented"
    return None
