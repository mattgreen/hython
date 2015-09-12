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
import Hython.Types
import qualified Hython.Statement as Statement

newtype Interpreter a = Interpreter { unwrap :: ContT Object (StateT InterpreterState IO) a }
                            deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

data InterpreterState = InterpreterState
    { stateEnv          :: Environment ObjectRef
    , stateFlow         :: ControlFlow InterpreterCont
    , stateResults      :: [Object]
    }

type InterpreterCont = Object -> Interpreter ()

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

    popEnvFrame = do
        env <- Interpreter $ gets stateEnv
        (bindings, env') <- pure $ Environment.pop env
        Interpreter $ modify $ \s -> s { stateEnv = env' }
        return bindings

    pushEnvFrame = Interpreter $ modify $ \s -> s { stateEnv = Environment.push (stateEnv s) }

    unbind name = do
        env <- Interpreter $ gets stateEnv
        case Environment.unbind name env of
            Left msg        -> raise "SyntaxError" msg
            Right newEnv    -> Interpreter $ modify $ \s -> s { stateEnv = newEnv }

instance MonadInterpreter Interpreter where
    evalBlock statements = mapM_ Statement.eval statements

    getControlCont ctrl = do
        flow <- Interpreter $ gets stateFlow
        case ctrl of
            BreakCont       -> return $ ControlFlow.getBreak flow
            ContinueCont    -> return $ ControlFlow.getContinue flow
            ReturnCont      -> return $ ControlFlow.getReturn flow

    popControlCont ctrl = do
        flow <- Interpreter $ gets stateFlow
        Interpreter $ modify $ \s -> s { stateFlow = action ctrl flow }
      where
        action (BreakCont) = ControlFlow.popBreak
        action (ContinueCont) = ControlFlow.popContinue
        action (ReturnCont) = ControlFlow.popReturn

    pushEvalResult obj = Interpreter $ modify $ \s -> s { stateResults = stateResults s ++ [obj] }

    pushControlCont ctrl c = do
        flow <- Interpreter $ gets stateFlow
        Interpreter $ modify $ \s -> s { stateFlow = action ctrl c flow }
      where
        action (BreakCont) = ControlFlow.pushBreak
        action (ContinueCont) = ControlFlow.pushContinue
        action (ReturnCont) = ControlFlow.pushReturn

    raise exceptionType msg = error (exceptionType ++ ": " ++ msg)

defaultInterpreterState :: IO InterpreterState
defaultInterpreterState = do
    builtinFns  <- mapM mkBuiltin builtinFunctions

    objClass    <- liftIO $ newIORef =<< newClass "object" [] []
    builtins    <- pure $ builtinFns ++ [(T.pack "object", objClass)]

    return InterpreterState {
        stateEnv = Environment.new builtins,
        stateFlow = ControlFlow.new,
        stateResults = []
    }
  where
    mkBuiltin name = do
        ref <- newIORef $ BuiltinFn name
        return (T.pack name, ref)

runInterpreter :: InterpreterState -> Text -> IO (Either String [Object], InterpreterState)
runInterpreter state code = case parse code of
    Left msg    -> return (Left msg, state)
    Right stmts -> do
        (_, newState) <- runStateT (runContT (unwrap $ run stmts) return) state
        let objs = stateResults newState
        return (Right objs, newState { stateResults = [] })
  where
    run stmts = do
        evalBlock stmts
        return None
