{-# LANGUAGE OverloadedStrings #-}
module Main (main)
where

import Prelude hiding (readFile)

import Control.Exception
import Data.Text (Text)
import Data.Text.IO (readFile)

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

runScript :: FilePath -> IO ()
runScript path = do
    code <- readFile path `catch` errorHandler path
    state <- defaultInterpreterState path

    (result, _) <- runInterpreter state code
    case result of
        Left msg    -> putStrLn msg
        Right _     -> return ()

  where
    errorHandler :: String -> IOError -> IO Text
    errorHandler _ err = do
        putStrLn $ printf "Unable to open '%s': file %s" path (ioeGetErrorString err)
        _ <- exitFailure
        return ""
