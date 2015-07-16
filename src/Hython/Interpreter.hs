{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hython.Interpreter
where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class as IO
import Control.Monad.State.Strict (StateT, gets, modify, evalStateT)
import Data.IORef
import qualified Data.Text as T

import Hython.Builtins (builtinFunctions)
import Hython.Environment (Environment)
import qualified Hython.Environment as Environment
import Hython.Monad
import Hython.Object
import qualified Hython.Statement as Statement

newtype Interpreter a = Interpreter (StateT InterpreterState IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

data InterpreterState = InterpreterState
    { stateEnv  :: Environment ObjectRef
    }

instance MonadInterpreter Interpreter where
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
            Left msg        -> raiseError "SyntaxError" msg
            Right newEnv    -> Interpreter $ modify $ \s -> s { stateEnv = newEnv }

    evalBlock statements = do
        results <- mapM Statement.eval statements
        return $ filter (/= None) results

    liftIO = IO.liftIO

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
            Left msg        -> raiseError "SyntaxError" msg
            Right newEnv    -> Interpreter $ modify $ \s -> s { stateEnv = newEnv }

    raiseError _ msg = error msg

runInterpreter :: Interpreter a -> IO a
runInterpreter (Interpreter i) = do
    builtinFns <- mapM mkBuiltin builtinFunctions
    let defaultState = InterpreterState {
        stateEnv = Environment.new builtinFns
    }

    evalStateT i defaultState
  where
    mkBuiltin name = do
        ref <- newIORef $ BuiltinFn name
        return (T.pack name, ref)
