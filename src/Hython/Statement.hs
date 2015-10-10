module Hython.Statement (eval)
where

import Control.Monad (unless)
import Control.Monad.Cont (callCC)
import Control.Monad.Fix (fix)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.IntMap as IntMap
import Data.IORef (modifyIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes)
import Safe (atMay)

import Language.Python

import Hython.Builtins (setAttr)
import Hython.Environment (bind, bindGlobal, bindNonlocal, pushEnvFrame, popEnvFrame, unbind)
import Hython.Expression (evalExpr)
import Hython.ControlFlow
import Hython.Types

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

eval (Assignment (Attribute targetExpr attr) expr) = do
    obj     <- evalExpr expr
    target  <- evalExpr targetExpr
    setAttr attr obj target

eval (Assignment (Name name) expr) = do
    value <- evalExpr expr
    bind name value

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
                    (left, right) <- pure $ splitAt (fromIntegral i) items
                    liftIO $ writeIORef ref (left ++ [value] ++ tail right)
                Nothing -> raise "IndexError" "index is out of range"

        _ -> raise "TypeError" "object does not support item assignment"

eval (Break) = do
    mcont <- getBreakCont
    case mcont of
        Just cont   -> cont None
        Nothing     -> raise "SyntaxError" "'break' outside loop"

eval (ClassDef name bases block) = do
    baseClasses <- catMaybes <$> mapM evalBase bases

    pushEnvFrame
    evalBlock block
    dict <- popEnvFrame

    cls <- newClass name baseClasses dict
    bind name cls
  where
    evalBase baseName = do
        obj <- evalExpr $ Name baseName
        case obj of
            Class info  -> return $ Just info
            _           -> do
                raise "TypeError" "error when evaluating bases"
                return Nothing

eval (Continue) = do
    mcont <- getContinueCont
    case mcont of
        Just cont   -> cont None
        Nothing     -> raise "SyntaxError" "'continue' outside loop"

eval (Del (Name name)) = do
    result <- unbind name
    case result of
        Left msg    -> raise "SyntaxError" msg
        Right _     -> return ()

eval (Del _) = raise "SystemError" "invalid del statement"

eval (Expression e) = do
    result <- evalExpr e
    pushEvalResult result

eval (FuncDef name params block) = do
    params' <- mapM evalParam params
    fn      <- newFunction name params' block
    bind name fn
  where
    evalParam (FormalParam param) = return $ NamedParam param
    evalParam (DefaultParam param expr) = do
        obj <- evalExpr expr
        return $ DefParam param obj
    evalParam (SplatParam param) = return $ SParam param
    evalParam (DoubleSplatParam param) = return $ DSParam param

eval (Global names) = mapM_ bindGlobal names

eval (If clauses elseBlock) = case clauses of
    (IfClause condition block : rest) -> do
        truthy  <- isTruthy =<< evalExpr condition
        if truthy
            then evalBlock block
            else eval (If rest elseBlock)
    [] -> evalBlock elseBlock

eval (Nonlocal names) = mapM_ bindNonlocal names

eval (Pass) = return ()

eval (Return expr) = do
    obj     <- evalExpr expr
    mcont   <- getReturnCont
    case mcont of
        Just cont   -> cont obj
        Nothing     -> raise "SyntaxError" "'return' outside of function"

eval (While condition block elseBlock) = do
    _ <- callCC $ \breakCont -> do
        pushBreakCont breakCont
        fix $ \loop ->
            callCC $ \continueCont -> do
                pushContinueCont continueCont

                truthy <- isTruthy =<< evalExpr condition
                unless truthy $ do
                    evalBlock elseBlock
                    breakCont None

                evalBlock block
                loop

    popContinueCont
    popBreakCont

eval _ = raise "SystemError" "statement not implemented"
