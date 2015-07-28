module Hython.Statement (eval)
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.IntMap as IntMap
import Data.IORef (modifyIORef)
import Data.Text (pack)

import Language.Python

import Hython.Builtins (toStr)
import Hython.Expression (evalExpr)
import Hython.Object

eval :: (MonadIO m, MonadInterpreter m) => Statement -> m Object
eval (Assert expr msgExpr) = do
    result  <- evalExpr expr
    msg     <- evalExpr msgExpr

    truthy  <- isTruthy result
    unless truthy $
        if isNone msg
            then raise "AssertionError" ""
            else do
                str <- toStr msg
                raise "AssertionError" str

    return None

eval (Assignment (Name name) expr) = do
    value <- evalExpr expr
    bind (pack name) value
    return None

eval (Assignment (Subscript targetExpr idxExpr) expr) = do
    target  <- evalExpr targetExpr
    index   <- evalExpr idxExpr
    value   <- evalExpr expr

    case (target, index) of
        (Dict ref, key) -> do
            h <- hash key
            liftIO $ modifyIORef ref $ IntMap.insert h (key, value)
            return None

        _ -> do
            raise "TypeError" "object does not support item assignment"
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
