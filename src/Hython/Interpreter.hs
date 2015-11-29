{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Hython.Interpreter
where

import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Cont (ContT, runContT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)

import Language.Python.Parser (parse)

import Hython.Builtins (builtinFunctions)
import Hython.ControlFlow (Flow, MonadFlow)
import qualified Hython.ControlFlow as ControlFlow
import qualified Hython.Environment as Environment
import qualified Hython.ExceptionHandling as ExceptionHandling
import Hython.Types
import qualified Hython.Statement as Statement

newtype Interpreter a = Interpreter { unwrap :: ContT Object (StateT InterpreterState IO) a }
                            deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

data InterpreterState = InterpreterState
    { stateEnv          :: Env
    , stateFlow         :: Flow Object Continuation
    , stateResults      :: [Object]
    }

type Continuation = Object -> Interpreter ()

instance MonadEnv Interpreter where
    getEnv      = Interpreter $ gets stateEnv
    putEnv env  = Interpreter $ modify $ \s -> s { stateEnv = env }

instance MonadFlow Object Continuation Interpreter where
    getFlow     = Interpreter $ gets stateFlow
    putFlow f   = Interpreter . modify $ \s -> s { stateFlow = f }

instance MonadInterpreter Interpreter where
    evalBlock statements = mapM_ Statement.eval statements
    pushEvalResult obj = Interpreter $ modify $ \s -> s { stateResults = stateResults s ++ [obj] }
    raise clsName desc = ExceptionHandling.raiseInternal (T.pack clsName) (T.pack desc)

defaultInterpreterState :: IO InterpreterState
defaultInterpreterState = do
    builtinFns      <- mapM mkBuiltin builtinFunctions

    object          <- defClass "object" []
    baseException   <- defClass "BaseException" [object]
    exception       <- defClass "Exception" [baseException]
    typeError       <- defClass "TypeError" [exception]
    nameError       <- defClass "NameError" [exception]

    objRef          <- mkBuiltinClass "object" object
    beRef           <- mkBuiltinClass "BaseException" baseException
    exRef           <- mkBuiltinClass "Exception" exception
    teRef           <- mkBuiltinClass "TypeError" typeError
    neRef           <- mkBuiltinClass "NameError" nameError

    builtins        <- pure $ builtinFns ++ [objRef, beRef, exRef, teRef, neRef]

    return InterpreterState {
        stateEnv = Environment.new builtins,
        stateFlow = ControlFlow.new defaultBreakHandler defaultContinueHandler defaultReturnHandler defaultExceptionHandler,
        stateResults = []
    }
  where
    classOf (Class info)    = Just info
    classOf _               = Nothing

    defClass name bases = newClass name (mapMaybe classOf bases) []

    mkBuiltin name = do
        ref <- newIORef $ BuiltinFn name
        return (name, ref)

    mkBuiltinClass name cls = do
        ref <- liftIO $ newIORef cls
        return (T.pack name, ref)

defaultBreakHandler :: Object -> Interpreter ()
defaultBreakHandler _ = raise "SyntaxError" "'break' outside loop"

defaultContinueHandler :: Object -> Interpreter ()
defaultContinueHandler _ = raise "SyntaxError" "'continue' not properly in loop"

defaultExceptionHandler :: Object -> Interpreter ()
defaultExceptionHandler ex = do
    case ex of
        (Object obj) -> do
            liftIO . putStrLn . T.unpack $ className (objectClass obj)
        _ -> liftIO $ putStrLn "o_O: raised a non-object exception"

    liftIO exitFailure

defaultReturnHandler :: Object -> Interpreter ()
defaultReturnHandler _ = raise "SyntaxError" "'return' outside function"

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
