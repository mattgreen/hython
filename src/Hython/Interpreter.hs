{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hython.Interpreter
where

import Control.Applicative
import Control.Monad.State
import Data.IORef
import Data.Maybe

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
    bind name obj = Interpreter $ do
        env <- gets stateEnv
        case Environment.lookup name env of
            Just ref -> liftIO $ writeIORef ref obj
            Nothing  -> do
                ref <- liftIO $ newIORef obj
                modify $ \s -> s { stateEnv = Environment.bind name ref (stateEnv s) }

    bindGlobal name = Interpreter $
        modify $ \s -> s { stateEnv = Environment.bindGlobal name (stateEnv s) }

    bindNonlocal name = Interpreter $ do
        env <- gets stateEnv
        case Environment.bindNonlocal name env of
            Left msg        -> error msg -- TODO: wrong
            Right newEnv    -> modify $ \s -> s { stateEnv = newEnv }

    evalBlock statements = do
        results <- mapM Statement.eval statements
        return $ catMaybes results

    lookupName name = Interpreter $ do
        env <- gets stateEnv
        case Environment.lookup name env of
            Just ref -> do
                obj <- liftIO $ readIORef ref
                return $ Just obj
            Nothing  -> return Nothing

    unbind name = Interpreter $ do
        env <- gets stateEnv

        case Environment.unbind name env of
            Left msg        -> error msg -- TODO: wrong
            Right newEnv    -> modify $ \s -> s { stateEnv = newEnv }

    raiseError _ msg = error msg

runInterpreter :: Interpreter a -> IO a
runInterpreter (Interpreter i) = do
    let defaultState = InterpreterState {
        stateEnv = Environment.new []
    }

    evalStateT i defaultState
