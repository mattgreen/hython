{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hython.Interpreter
where

import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Cont (ContT, runContT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import Language.Python.Parser (parse)

import Hython.Builtins (builtinFunctions)
import Hython.ControlFlow (ControlFlow)
import qualified Hython.ControlFlow as ControlFlow
import Hython.Environment (Environment)
import qualified Hython.Environment as Environment
import Hython.Object
import qualified Hython.Statement as Statement

newtype Interpreter a = Interpreter { unInterpreter :: ContT [Object] (StateT InterpreterState IO) a }
                            deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

data InterpreterState = InterpreterState
    { stateEnv          :: Environment ObjectRef
    , stateFlow         :: ControlFlow Interpreter
    }

instance MonadEnvironment Interpreter where
    bind name obj = do
        env <- Interpreter $ gets stateEnv
        case Environment.lookup name env of
            Just ref -> liftIO $ writeIORef ref obj
            Nothing  -> do
                ref <- liftIO $ newIORef obj
                Interpreter $ modify $ \s -> s { stateEnv = Environment.bind name ref (stateEnv s) }

    bindGlobal name = Interpreter $
        modify $ \s -> s { stateEnv = Environment.bindGlobal name (stateEnv s) }

    bindNonlocal name = do
        env <- Interpreter $ gets stateEnv
        case Environment.bindNonlocal name env of
            Left msg        -> raise "SyntaxError" msg
            Right newEnv    -> Interpreter $ modify $ \s -> s { stateEnv = newEnv }

    lookupName name = do
        env <- Interpreter $ gets stateEnv
        case Environment.lookup name env of
            Just ref -> do
                obj <- liftIO $ readIORef ref
                return $ Just obj
            Nothing  -> return Nothing

    unbind name = do
        env <- Interpreter $ gets stateEnv
        case Environment.unbind name env of
            Left msg        -> raise "SyntaxError" msg
            Right newEnv    -> Interpreter $ modify $ \s -> s { stateEnv = newEnv }

instance MonadInterpreter Interpreter where
    evalBlock statements = do
        results <- mapM Statement.eval statements
        return $ filter (not . isNone) results

    getControlCont ctrl = do
        flow <- Interpreter $ gets stateFlow
        case ctrl of
            BreakCont    -> return $ ControlFlow.getBreak flow
            ContinueCont -> return $ ControlFlow.getContinue flow

    popControlCont _ = do
        flow <- Interpreter $ gets stateFlow
        Interpreter $ modify $ \s -> s { stateFlow = ControlFlow.popBreak flow }

    pushControlCont ctrl c = do
        flow <- Interpreter $ gets stateFlow
        Interpreter $ modify $ \s -> s { stateFlow = action ctrl c flow }
      where
        action (BreakCont) = ControlFlow.pushBreak
        action (ContinueCont) = ControlFlow.pushContinue

    raise exceptionType msg = error (exceptionType ++ ": " ++ msg)

defaultInterpreterState :: IO InterpreterState
defaultInterpreterState = do
    builtinFns <- mapM mkBuiltin builtinFunctions
    return InterpreterState {
        stateEnv = Environment.new builtinFns,
        stateFlow = ControlFlow.new
    }
  where
    mkBuiltin name = do
        ref <- newIORef $ BuiltinFn name
        return (T.pack name, ref)

runInterpreter :: InterpreterState -> Text -> IO (Either String [Object], InterpreterState)
runInterpreter state code = case parse code of
    Left msg    -> return (Left msg, state)
    Right stmts -> do
        (objs, newState) <- runStateT (runContT (unInterpreter $ evalBlock stmts) return) state
        return (Right objs, newState)
