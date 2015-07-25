module Main (main)
where

import Control.Exception

import System.Environment
import System.Exit (exitFailure)
import System.IO.Error
import Text.Printf

import Hython.Interpreter (defaultInterpreterState, runInterpreter)
import REPL (runREPL)

main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> runREPL
        [filename]  -> runScript filename
        _           -> do
            putStrLn "Usage: hython <filename>"
            exitFailure

runScript :: String -> IO ()
runScript filename = do
    code <- readFile filename `catch` errorHandler filename
    state <- defaultInterpreterState

    (result, _) <- runInterpreter state code
    case result of
        Left msg    -> putStrLn msg
        Right _     -> return ()

  where
    errorHandler :: String -> IOError -> IO String
    errorHandler _ err = do
        putStrLn $ printf "Unable to open '%s': file %s" filename (ioeGetErrorString err)
        _ <- exitFailure
        return ""
