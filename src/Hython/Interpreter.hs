{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

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
import Hython.ControlFlow (Flow, MonadFlow)
import qualified Hython.ControlFlow as ControlFlow
import qualified Hython.Environment as Environment
import Hython.Types
import qualified Hython.Statement as Statement

newtype Interpreter a = Interpreter { unwrap :: ContT Object (StateT InterpreterState IO) a }
                            deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

data InterpreterState = InterpreterState
    { stateEnv          :: Env
    , stateFlow         :: Flow Continuation
    , stateResults      :: [Object]
    }

type Continuation = Object -> Interpreter ()

instance MonadEnv Interpreter where
    getEnv      = Interpreter $ gets stateEnv
    putEnv env  = Interpreter $ modify $ \s -> s { stateEnv = env }

instance MonadFlow Continuation Interpreter where
    getFlow     = Interpreter $ gets stateFlow
    putFlow f   = Interpreter . modify $ \s -> s { stateFlow = f }

instance MonadInterpreter Interpreter where
    evalBlock statements = mapM_ Statement.eval statements
    pushEvalResult obj = Interpreter $ modify $ \s -> s { stateResults = stateResults s ++ [obj] }
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
