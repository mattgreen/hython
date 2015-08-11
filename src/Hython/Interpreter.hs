{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, RankNTypes #-}

module Hython.Interpreter
where

import Control.Applicative
import Control.Monad.CC
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T

import Language.Python.Parser (parse)

import Hython.Builtins (builtinFunctions)
import Hython.Environment (Environment)
import qualified Hython.Environment as Environment
import Hython.Object
import qualified Hython.Statement as Statement

newtype Interpreter ans a = Interpreter { unInterpreter :: CCT ans (StateT InterpreterState IO) a }
                            deriving (Functor, Applicative, Monad, MonadIO)

instance MonadDelimitedCont (Prompt ans) (SubCont ans (StateT InterpreterState IO)) (Interpreter ans) where
    newPrompt = Interpreter $ newPrompt
    pushPrompt p x = Interpreter (pushPrompt p (unInterpreter x))
    withSubCont p f = Interpreter (withSubCont p (unInterpreter . f))
    pushSubCont s x = Interpreter (pushSubCont s (unInterpreter x))

data InterpreterState = InterpreterState
    { stateEnv  :: Environment ObjectRef
    }

instance MonadEnvironment (Interpreter ans) where
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

instance MonadInterpreter (Interpreter ans) where
    evalBlock statements = do
        results <- mapM Statement.eval statements
        return $ filter (not . isNone) results

    raise exceptionType msg = error (exceptionType ++ ": " ++ msg)

defaultInterpreterState :: IO InterpreterState
defaultInterpreterState = do
    builtinFns <- mapM mkBuiltin builtinFunctions
    return InterpreterState {
        stateEnv = Environment.new builtinFns
    }
  where
    mkBuiltin name = do
        ref <- newIORef $ BuiltinFn name
        return (T.pack name, ref)

runInterpreter :: InterpreterState -> Text -> IO (Either String [Object], InterpreterState)
runInterpreter state code = case parse code of
    Left msg    -> return (Left msg, state)
    Right stmts -> do
        (objs, newState) <- flip runStateT state (runCCT (unInterpreter $ evalBlock stmts))
        return (Right objs, newState)
