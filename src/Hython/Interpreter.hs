{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Hython.Interpreter
where

import Prelude hiding (readFile)

import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Cont (ContT, runContT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import Data.List (find)
import Data.Text (Text)
import Data.Text.IO (readFile)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesFileExist, getDirectoryContents)
import System.Exit (exitFailure)
import System.Environment.Executable (splitExecutablePath)
import System.FilePath

import Language.Python.Parser (parse)

import Hython.Builtins (builtinFunctions)
import qualified Hython.Call as Call
import Hython.ControlFlow (Flow, MonadFlow)
import qualified Hython.ControlFlow as ControlFlow
import qualified Hython.Environment as Environment
import qualified Hython.ExceptionHandling as ExceptionHandling
import Hython.Ref
import Hython.Types
import qualified Hython.Statement as Statement

newtype Interpreter a = Interpreter { unwrap :: ContT Object (StateT InterpreterState IO) a }
                            deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

data InterpreterState = InterpreterState
    { stateEnv          :: Env
    , stateFlow         :: Flow Object Continuation
    , stateNew          :: Bool
    , stateCurModule    :: ModuleInfo
    , stateModules      :: [ModuleInfo]
    , stateResults      :: [String]
    }

type Continuation = Object -> Interpreter ()

instance Environment.MonadEnv Object Interpreter where
    getEnv      = Interpreter $ gets stateEnv
    putEnv env  = Interpreter $ modify $ \s -> s { stateEnv = env }

instance MonadFlow Object Continuation Interpreter where
    getFlow     = Interpreter $ gets stateFlow
    putFlow f   = Interpreter . modify $ \s -> s { stateFlow = f }

instance MonadInterpreter Interpreter where
    evalBlock = mapM_ Statement.eval
    pushEvalResult str = Interpreter $ modify $ \s -> s { stateResults = stateResults s ++ [str] }
    invoke obj method args = Call.invoke obj method args []
    new = Call.new
    raise clsName desc = ExceptionHandling.raiseInternal (T.pack clsName) (T.pack desc)
    getCurrentModule = Interpreter . gets $ stateCurModule
    getModuleByPath path = do
        modules <- Interpreter . gets $ stateModules
        return $ find (\m -> modulePath m == path) modules
    setCurrentModule info = do
        modules <- Interpreter . gets $ stateModules

        Interpreter . modify $ \s -> s { stateCurModule = info }
        unless (info `elem` modules) $
            Interpreter . modify $ \s -> s { stateModules = info : stateModules s }

defaultInterpreterState :: FilePath -> IO InterpreterState
defaultInterpreterState path = do
    builtinFns      <- mapM mkBuiltin builtinFunctions

    fullPath        <- getModulePath
    (Module main)   <- newModule "__main__" fullPath
    objCls          <- newClass "object" [] [] main
    objRef          <- mkBuiltinClass "object" objCls
    builtins        <- pure $ builtinFns ++ [objRef]
    env             <- Environment.new builtins

    return InterpreterState {
        stateEnv = env,
        stateFlow = ControlFlow.new defaultBreakHandler defaultContinueHandler defaultReturnHandler defaultExceptionHandler,
        stateCurModule = main,
        stateModules = [main],
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

    getModulePath
      | path == "<repl>"    = return "<repl>"
      | otherwise           = canonicalizePath path

defaultBreakHandler :: Object -> Interpreter ()
defaultBreakHandler _ = raise "SyntaxError" "'break' outside loop"

defaultContinueHandler :: Object -> Interpreter ()
defaultContinueHandler _ = raise "SyntaxError" "'continue' not properly in loop"

defaultExceptionHandler :: Object -> Interpreter ()
defaultExceptionHandler ex = do
    case ex of
        Object info -> do
            msg <- toStr =<< invoke ex "__str__" []

            liftIO $ do
                putStr . T.unpack . className . objectClass $ info
                putStr ": "
                putStrLn msg
        _ -> liftIO $ putStrLn "SystemError: uncaught non-object exception"

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
        libDir      <- canonicalizePath $ exeDir </> "lib" </> "builtins"
        entries     <- getDirectoryContents libDir

        filterM doesFileExist $ map (libDir </>) entries

runInterpreter :: InterpreterState -> Text -> IO (Either String [String], InterpreterState)
runInterpreter state code = case parse code of
    Left msg    -> return (Left msg, state)
    Right stmts -> do
        let firstTime = stateNew state

        (_, newState) <- runStateT (runContT (unwrap $ run firstTime stmts) return) state
        let results = stateResults newState
        return (Right results, newState { stateNew = False, stateResults = [] })
  where
    run firstTime stmts = do
        when firstTime $ do
            loadBuiltinModules
            Environment.moveLocalsToBuiltins

        evalBlock stmts
        return None
