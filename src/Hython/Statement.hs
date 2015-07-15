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

eval (Del (Name name)) = do
    unbind (pack name)
    return Nothing

eval (Del _) = do
    raiseError "SystemError" "invalid del statement"
    return Nothing

eval (Expression e) = do
    obj <- evalExpr e
    return $ Just obj

eval (Global names) = do
    mapM_ (bindGlobal . pack) names
    return Nothing

eval (If clauses elseBlock) = case clauses of
    [] -> do
        _ <- evalBlock elseBlock
        return Nothing
    (IfClause condition block : rest) -> do
        result <- evalExpr condition
        if isTruthy result
            then do
                _ <- evalBlock block
                return Nothing
            else eval (If rest elseBlock)

eval (Nonlocal names) = do
    mapM_ (bindNonlocal . pack) names
    return Nothing

eval (Pass) = return Nothing

eval _ = do
    raiseError "SystemError" "statement not implemented"
    return Nothing
