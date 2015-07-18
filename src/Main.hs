module Main (main)
where

import Control.Exception
import Control.Monad.IO.Class (liftIO)

import System.Environment
import System.Exit (exitFailure)
import System.IO.Error
import Text.Printf

import System.Console.Haskeline hiding (catch)

import Hython.Builtins (toStr)
import Hython.Interpreter (defaultInterpreterState, runInterpreter)

main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> runREPL
        [filename]  -> runScript filename
        _           -> do
            putStrLn "Usage: hython <filename>"
            exitFailure

runREPL :: IO ()
runREPL = do
    state <- defaultInterpreterState
    runInputT defaultSettings (loop state)
  where
    loop state = do
        input <- getInputLine ">>> "
        case input of
            Nothing         -> return ()
            Just "quit()"   -> return ()
            Just line -> do
                (result, newState) <- liftIO $ runInterpreter state line
                case result of
                    Left s      -> outputStrLn s
                    Right objs  -> mapM_ (outputStrLn . toStr) objs

                loop newState

runScript :: String -> IO ()
runScript filename = do
    code <- readFile filename `catch` errorHandler filename

    state <- defaultInterpreterState
    _ <- runInterpreter state code
    return ()

  where
    errorHandler :: String -> IOError -> IO String
    errorHandler _ err = do
        putStrLn $ printf "Unable to open '%s': file %s" filename (ioeGetErrorString err)
        _ <- exitFailure
        return ""
