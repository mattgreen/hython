module Hython.Statement (eval)
where

import Control.Monad (forM, unless)
import Control.Monad.Cont (callCC)
import Control.Monad.Fix (fix)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.IntMap as IntMap
import Data.IORef (modifyIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Safe (atMay)

import Language.Python

import Hython.Builtins (isInstance, setAttr)
import Hython.ControlFlow
import Hython.Environment (bind, bindGlobal, bindNonlocal, getFrameDepth, pushEnvFrame, popEnvFrame, unbind, unwindTo)
import Hython.Expression (evalExpr)
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
    handler <- getBreakHandler
    handler None

eval (ClassDef name bases block) = do
    baseClasses <- catMaybes <$> mapM evalBase bases

    pushEnvFrame []
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
    handler <- getContinueHandler
    handler None

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

eval (Raise expr _from) = do
    exception   <- evalExpr expr
    handler     <- getExceptHandler

    setCurrentException exception
    handler exception

eval (Reraise) = do
    mexception  <- getCurrentException
    handler     <- getExceptHandler

    case mexception of
        Just exception  -> handler exception
        Nothing         -> return ()

eval (Return expr) = do
    obj     <- evalExpr expr
    handler <- getReturnHandler

    handler obj

eval (Try clauses block elseBlock finallyBlock) = do
    -- Snapshot control flow state
    breakHandler    <- getBreakHandler
    continueHandler <- getContinueHandler
    returnHandler   <- getReturnHandler
    exceptHandler   <- getExceptHandler
    frameDepth      <- getFrameDepth

    -- Evaluate exception handler clauses
    handlers <- forM clauses $ \(ExceptClause expr name handlerBlock) -> do
        mcls <- evalExpr expr
        case mcls of
            (Class cls) -> return . Just $ ExceptionHandler name cls handlerBlock
            _           -> do
                raise "SyntaxError" "invalid class in except block header"
                return Nothing

    --
    exception <- callCC $ \handler -> do
        setExceptHandler handler

        -- Install control flow handlers. They're responsible for:
        -- 1. Uninstalling themselves (they are one-shot)
        -- 2. Restoring the exception handler of the try block
        -- 3. Running the finally block
        -- 4. Calling the original handler
        setBreakHandler $ \obj -> do
            setBreakHandler breakHandler
            setExceptHandler exceptHandler
            evalBlock finallyBlock
            breakHandler obj

        setContinueHandler $ \obj -> do
            setContinueHandler continueHandler
            setExceptHandler exceptHandler
            evalBlock finallyBlock
            continueHandler obj

        setReturnHandler $ \obj -> do
            setReturnHandler returnHandler
            setExceptHandler exceptHandler
            evalBlock finallyBlock
            returnHandler obj

        evalBlock block
        return None

    -- Between the try block and the except blocks here
    -- Revert all control flow state to be as it was when we started
    setExceptHandler exceptHandler
    setBreakHandler breakHandler
    setContinueHandler continueHandler
    setReturnHandler returnHandler

    -- Unwind the stack to this try block's depth
    {-liftIO . print $ frameDepth-}
    unwindTo frameDepth
    popFramesTo frameDepth

    -- If we have an exception, look for exception handler in this try/except block
    handled <- if isNone exception
        then do
            evalBlock elseBlock
            return True
        else
            case find (clauseMatches exception) (catMaybes handlers) of
                Just (ExceptionHandler name _ handlerBlock) -> do
                    unless (T.null name) $
                        bind name exception

                    setExceptHandler $ \obj -> do
                        setExceptHandler exceptHandler
                        evalBlock finallyBlock
                        exceptHandler obj

                    evalBlock handlerBlock

                    setExceptHandler exceptHandler

                    return True
                Nothing -> return False

    evalBlock finallyBlock

    unless handled $
        exceptHandler exception
  where
    clauseMatches obj (ExceptionHandler _ info _) = isInstance obj info

eval (While condition block elseBlock) = do
    breakHandler    <- getBreakHandler
    continueHandler <- getContinueHandler
    exceptHandler   <- getExceptHandler

    _ <- callCC $ \brk ->
        fix $ \loop -> do
            _ <- callCC $ \continue -> do
                setBreakHandler $ \obj -> do
                    setExceptHandler exceptHandler
                    brk obj

                setContinueHandler $ \obj -> do
                    setExceptHandler exceptHandler
                    continue obj

                truthy <- isTruthy =<< evalExpr condition
                unless truthy $ do
                    evalBlock elseBlock
                    brk None

                evalBlock block
                return None

            loop

    setContinueHandler continueHandler
    setBreakHandler breakHandler

eval _ = raise "SystemError" "statement not implemented"
