module REPL (runREPL) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)

import System.Console.Haskeline

import Hython.Builtins (toStr)
import Hython.Interpreter (defaultInterpreterState, runInterpreter)
import Hython.Object (isNone)

runREPL :: IO ()
runREPL = do
    state <- defaultInterpreterState
    runInputT settings (loop state)
  where
    loop state = do
        input <- getInputLine ">>> "
        case input of
            Nothing         -> return ()
            Just "quit()"   -> return ()
            Just line -> do
                (result, newState) <- liftIO $ runInterpreter state (pack line)
                case result of
                    Left s      -> outputStrLn s
                    Right objs  -> do
                        strs <- mapM toStr (filter (not . isNone) objs)
                        mapM_ outputStrLn strs

                loop newState

    settings = defaultSettings { historyFile = Just ".hython_history" }
