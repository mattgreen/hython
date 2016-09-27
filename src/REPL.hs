module REPL (runREPL) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)

import System.Console.Haskeline

import Hython.Interpreter (defaultInterpreterState, interpret)

runREPL :: IO ()
runREPL = do
    state <- defaultInterpreterState "<repl>"
    runInputT settings (loop state)
  where
    loop state = do
        input <- getInputLine ">>> "
        case input of
            Nothing         -> return ()
            Just "quit()"   -> return ()
            Just line -> do
                (result, newState) <- liftIO $ interpret state (pack line)
                case result of
                    Left s      -> outputStrLn s
                    Right strs  -> mapM_ outputStrLn strs

                loop newState

    settings = defaultSettings { historyFile = Just ".hython_history" }
