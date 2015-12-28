{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Hython.Interpreter
where

import Prelude hiding (readFile)

import Control.Monad (filterM, forM_, when)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Cont (ContT, runContT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesFileExist, getDirectoryContents)
import System.Exit (exitFailure)
import System.Environment.Executable (splitExecutablePath)
import System.FilePath

import Language.Python.Parser (parse)

import Hython.Builtins (asStr, builtinFunctions, getAttr)
import Hython.Call (call)
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
    , stateNew          :: Bool
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

    objCls          <- newClass "object" [] []
    objRef          <- mkBuiltinClass "object" objCls
    builtins        <- pure $ builtinFns ++ [objRef]

    return InterpreterState {
        stateEnv = Environment.new builtins,
        stateFlow = ControlFlow.new defaultBreakHandler defaultContinueHandler defaultReturnHandler defaultExceptionHandler,
        stateNew = True,
        stateResults = []
    }
  where
    mkBuiltin name = do
        ref <- newRef $ BuiltinFn name
        return (name, ref)

    mkBuiltinClass name cls = do
        ref <- newRef cls
        return (T.pack name, ref)

defaultBreakHandler :: Object -> Interpreter ()
defaultBreakHandler _ = raise "SyntaxError" "'break' outside loop"

defaultContinueHandler :: Object -> Interpreter ()
defaultContinueHandler _ = raise "SyntaxError" "'continue' not properly in loop"

defaultExceptionHandler :: Object -> Interpreter ()
defaultExceptionHandler ex = do
    case ex of
        obj@(Object info) -> do
            mstr <- getAttr "__str__" obj
            msg  <- case mstr of
                Just method -> call method [] []
                Nothing     -> newString "object does not define __str__"

            liftIO $ do
                putStr . T.unpack . className . objectClass $ info
                putStr ": "
                putStrLn =<< asStr msg
        _ -> liftIO $ putStrLn "o_O: raised a non-object exception"

    liftIO exitFailure

defaultReturnHandler :: Object -> Interpreter ()
defaultReturnHandler _ = raise "SyntaxError" "'return' outside function"

loadBuiltinModules :: Interpreter ()
loadBuiltinModules = do
    modulePaths <- liftIO getModulePaths

    forM_ modulePaths $ \path -> do
        code <- liftIO . readFile $ path
        case parse code of
            Left err    -> raise "SyntaxError" err
            Right stmts -> evalBlock stmts
  where
    getModulePaths = do
        exeDir      <- fst <$> splitExecutablePath
        libDir      <- canonicalizePath $ exeDir </> "lib"
        entries     <- getDirectoryContents libDir

        filterM doesFileExist $ map (libDir </>) entries

runInterpreter :: InterpreterState -> Text -> IO (Either String [Object], InterpreterState)
runInterpreter state code = case parse code of
    Left msg    -> return (Left msg, state)
    Right stmts -> do
        let new = stateNew state

        (_, newState) <- runStateT (runContT (unwrap $ run new stmts) return) state
        let objs = stateResults newState
        return (Right objs, newState { stateNew = False, stateResults = [] })
  where
    run new stmts = do
        when new
            loadBuiltinModules

        evalBlock stmts
        return None
