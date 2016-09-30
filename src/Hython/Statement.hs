{-# LANGUAGE OverloadedStrings #-}

module Hython.Statement (eval)
where

import Control.Monad (forM, forM_, unless, when, void, zipWithM_)
import Control.Monad.Cont (callCC)
import Control.Monad.Fix (fix)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Hashable as H
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T

import Language.Python

import Hython.Builtins (isInstance, len, setAttr)
import Hython.Class (isSubClass)
import Hython.ControlFlow
import Hython.Environment
import Hython.ExceptionHandling (raiseExternal)
import Hython.Expression (evalExpr, evalParam)
import qualified Hython.Module as Module
import Hython.Ref
import Hython.Types

eval :: (MonadIO m, MonadCont m, MonadEnv Object m, MonadInterpreter m) => Statement -> m ()
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

eval (Assignment (TupleDef targetExprs) expr) = do
    source      <- evalExpr expr
    sourceLen   <- len source

    when (sourceLen > targetLen) $
        raise "ValueError" "too many values to unpack"
    when (targetLen > sourceLen) $
        raise "ValueError" "not enough values to unpack"

    zipWithM_ (bindName source) targetExprs [0 .. ]
  where
    bindName source target idx = case target of
        Name name -> do
            index <- newInt idx
            value <- invoke source "__getitem__" [index]
            bind name value
        _ -> raise "SyntaxError" "can only destructure to names"

    targetLen = length targetExprs

eval (Assignment (Subscript targetExpr keyExpr) expr) = do
    value   <- evalExpr expr
    target  <- evalExpr targetExpr
    key     <- evalExpr keyExpr

    void $ invoke target "__setitem__" [key, value]

eval (Break) = do
    handler <- getBreakHandler
    handler None

eval (ClassDef name bases block) = do
    baseClasses <- catMaybes <$> mapM evalBase bases

    env             <- getEnv
    tempEnv         <- putEnvWithBindings [] env
    evalBlock block
    dict            <- restoreEnv tempEnv
    currentModule   <- getCurrentModule

    cls <- newClass name baseClasses dict currentModule
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

eval (Del (Subscript expr idxExpr)) = do
    target  <- evalExpr expr
    index   <- evalExpr idxExpr

    void $ invoke target "__delitem__" [index]

eval (Del _) = raise "SystemError" "invalid del statement"

eval (Expression e) = do
    result <- evalExpr e

    unless (isNone result) $
        pushEvalResult =<< toStr result

eval s@(For target iterableExpr block elseBlock) = eval (Try clauses tryBody [] [])
  where
    tryBody = [iteratorCreate, whileStmt]
    iteratorCreate = Assignment iteratorName (Call (Name (T.pack "iter")) [Arg iterableExpr])
    whileStmt = While (Constant $ ConstantBool True) (Assignment target iteratorNextCall : block) []
    iteratorNextCall = Call (Attribute iteratorName (T.pack "__next__")) []
    iteratorName = Name . T.pack $ "__iterator_" ++ show (abs $ H.hash (show s))
    clauses = [ExceptClause (Name $ T.pack "StopIteration") T.empty elseBlock]

eval (FuncDef name params block) = do
    params'     <- mapM evalParam params
    env         <- getClosingEnv
    fn          <- newFunction name params' block env
    bind name fn

eval (Global names) = mapM_ bindGlobal names

eval (If clauses elseBlock) = case clauses of
    (IfClause condition block : rest) -> do
        truthy  <- isTruthy =<< evalExpr condition
        if truthy
            then evalBlock block
            else eval (If rest elseBlock)
    [] -> evalBlock elseBlock

eval (Import exprs) = forM_ exprs $ \expr ->
    case expr of
        Name name   -> do
            minfo <- Module.load (T.unpack name)
            case minfo of
                Right info  -> bind name (Module info)
                Left err    -> importError err
        (As (Name path) (Name name))    -> do
            minfo <- Module.load (T.unpack path)
            case minfo of
                Right info  -> bind name (Module info)
                Left err    -> importError err
        _           -> raise "SystemError" "unhandled import statement"

eval (ImportFrom (RelativeImport _ (Name path)) [Glob]) = do
    minfo <- Module.load (T.unpack path)
    case minfo of
        Right info  -> do
            vars <- readRef $ moduleDict info
            bindMany (Map.toList vars)
        Left err    -> importError err

eval (Nonlocal names) = mapM_ bindNonlocal names

eval (Pass) = return ()

eval (Raise expr _from) = raiseExternal =<< evalExpr expr

eval (Reraise) = do
    mexception  <- getCurrentException
    handler     <- getExceptionHandler

    case mexception of
        Just exception  -> handler exception
        Nothing         -> return ()

eval (Return expr) = do
    obj     <- evalExpr expr
    handler <- getReturnHandler

    handler obj

eval (Try clauses block elseBlock finallyBlock) = do
    -- Snapshot state
    env             <- getEnv
    breakHandler    <- getBreakHandler
    continueHandler <- getContinueHandler
    returnHandler   <- getReturnHandler
    exceptHandler   <- getExceptionHandler
    frameDepth      <- getFrameDepth

    -- Evaluate exception handler clauses
    handlers <- forM clauses $ \(ExceptClause expr name handlerBlock) -> do
        mcls <- evalExpr expr
        mBaseClass <- lookupName (T.pack "BaseException")
        case (mcls, mBaseClass) of
            (Class cls, Just (Class baseClass)) | isSubClass cls baseClass ->
                return . Just $ ExceptionHandler name cls handlerBlock
            _ -> do
                raise "TypeError" "catching classes that do not inherit from BaseException is not allowed"
                return Nothing

    exception <- callCC $ \handler -> do
        setExceptionHandler handler

        -- Install control flow handlers. They're responsible for:
        -- 1. Uninstalling themselves (they are one-shot)
        -- 2. Restoring the exception handler of the try block
        -- 3. Running the finally block
        -- 4. Calling the original handler
        setBreakHandler $ \obj -> do
            setBreakHandler breakHandler
            setExceptionHandler exceptHandler
            evalBlock finallyBlock
            breakHandler obj

        setContinueHandler $ \obj -> do
            setContinueHandler continueHandler
            setExceptionHandler exceptHandler
            evalBlock finallyBlock
            continueHandler obj

        setReturnHandler $ \obj -> do
            setReturnHandler returnHandler
            setExceptionHandler exceptHandler
            evalBlock finallyBlock
            returnHandler obj

        evalBlock block
        return None

    -- Between the try block and the except blocks here
    -- Revert all control flow state to be as it was when we started
    setExceptionHandler exceptHandler
    setBreakHandler breakHandler
    setContinueHandler continueHandler
    setReturnHandler returnHandler

    -- Unwind the stack to this try block's depth
    putEnv env
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

                    setExceptionHandler $ \obj -> do
                        setExceptionHandler exceptHandler
                        evalBlock finallyBlock
                        exceptHandler obj

                    evalBlock handlerBlock

                    setExceptionHandler exceptHandler

                    return True
                Nothing -> return False

    evalBlock finallyBlock

    if handled
        then clearCurrentException
        else exceptHandler exception
  where
    clauseMatches obj (ExceptionHandler _ info _) = isInstance obj info

eval (While condition block elseBlock) = do
    breakHandler    <- getBreakHandler
    continueHandler <- getContinueHandler
    exceptHandler   <- getExceptionHandler

    _ <- callCC $ \brk ->
        fix $ \loop -> do
            _ <- callCC $ \continue -> do
                setBreakHandler $ \obj -> do
                    setExceptionHandler exceptHandler
                    brk obj

                setContinueHandler $ \obj -> do
                    setExceptionHandler exceptHandler
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

eval s@(With (WithExpression expr name) block) = evalBlock desugared
  where
    target
      | T.null name = T.pack ("__hython_with_" ++ (show . H.hash . show $ s))
      | otherwise   = name
    desugared =
        [ Assignment (Name target) (Call (Attribute expr "__enter__") [])
        , Try clauses block elseBlock []
        ]
    clauses =
        [ ExceptClause (Name "BaseException") exceptName
            [ If
                [IfClause (callExit [Arg exceptClass, Arg $ Name exceptName, Arg traceback]) [Pass]]
                [Reraise]
            ]
        ]
    elseBlock = [Expression $ callExit [Arg none, Arg none, Arg none]]
    callExit = Call (Attribute (Name target) "__exit__")
    exceptName = T.pack $ "__hython_except__" ++ (show . H.hash . show $ s)
    exceptClass = Attribute (Name exceptName) "__class__"
    traceback = Call (Name "traceback") []
    none = Constant ConstantNone

eval _ = raise "SystemError" "statement not implemented"

importError :: MonadInterpreter m => Module.LoadError -> m ()
importError (Module.SyntaxError msg) = raise "SyntaxError" msg
importError (Module.IOError msg) = raise "SystemError" msg
