module Hython.Statement (eval)
where

import Control.Monad (unless)
import Control.Monad.Cont (callCC)
import Control.Monad.Fix (fix)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.IntMap as IntMap
import Data.IORef (modifyIORef, readIORef, writeIORef)
import Data.Text (pack)
import Safe (atMay)

import Language.Python

import Hython.Builtins (toStr)
import Hython.Expression (evalExpr)
import Hython.Object

eval :: (MonadIO m, MonadCont m, MonadInterpreter m) => Statement -> m ()
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

eval (Assignment (Name name) expr) = do
    value <- evalExpr expr
    bind (pack name) value

eval (Assignment (Subscript targetExpr idxExpr) expr) = do
    target  <- evalExpr targetExpr
    index   <- evalExpr idxExpr
    value   <- evalExpr expr

    case (target, index) of
        (Dict ref, key) -> do
            h <- hash key
            liftIO $ modifyIORef ref $ IntMap.insert h (key, value)

        (List ref, Int i) -> do
            items <- liftIO $ readIORef ref
            case atMay items (fromIntegral i) of
                Just _ -> do
                    let (left, right) = splitAt (fromIntegral i) items
                    liftIO $ writeIORef ref (left ++ [value] ++ tail right)
                Nothing -> raise "IndexError" "index is out of range"

        _ -> raise "TypeError" "object does not support item assignment"

eval (Break) = do
    mcont <- getControlCont BreakCont
    case mcont of
        Just cont   -> cont None
        Nothing     -> raise "SyntaxError" "'break' outside loop"

eval (Continue) = do
    mcont <- getControlCont ContinueCont
    case mcont of
        Just cont   -> cont None
        Nothing     -> raise "SyntaxError" "'continue' outside loop"

eval (Del (Name name)) = unbind (pack name)

eval (Del _) = raise "SystemError" "invalid del statement"

eval (Expression e) = do
    result <- evalExpr e
    pushEvalResult result

eval (FuncDef name params block) = do
    fn <- newFunction name params block
    bind (pack name) fn

eval (Global names) = mapM_ (bindGlobal . pack) names

eval (If clauses elseBlock) = case clauses of
    [] -> evalBlock elseBlock
    (IfClause condition block : rest) -> do
        result  <- evalExpr condition
        truthy  <- isTruthy result
        if truthy
            then evalBlock block
            else eval (If rest elseBlock)

eval (Nonlocal names) = mapM_ (bindNonlocal . pack) names

eval (Pass) = return ()

eval (Return expr) = do
    obj <- evalExpr expr
    mcont <- getControlCont ReturnCont
    case mcont of
        Just cont   -> cont obj
        Nothing     -> raise "SyntaxError" "'return' outside of function"

eval (While condition block elseBlock) = do
    _ <- callCC $ \breakCont -> do
        pushControlCont BreakCont breakCont
        fix $ \loop ->
            callCC $ \continueCont -> do
                pushControlCont ContinueCont continueCont
                result <- evalExpr condition
                truthy <- isTruthy result
                unless truthy $ do
                    evalBlock elseBlock
                    breakCont None
                evalBlock block
                loop

    popControlCont ContinueCont
    popControlCont BreakCont

eval _ = raise "SystemError" "statement not implemented"
