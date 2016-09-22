{-# LANGUAGE OverloadedStrings #-}
module Main (main)
where

import Prelude hiding (readFile)

import Control.Exception
import Data.Text (Text)
import Data.Text.IO (readFile)

import System.Environment
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Error

import Hython.Interpreter (defaultInterpreterState, interpret)
import REPL (runREPL)

main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> runREPL
        [filename]  -> runScript filename
        _           -> do
            progName <- getProgName
            hPutStrLn stderr $ "Usage: " ++ progName ++ " [filename]"
            exitFailure

runScript :: FilePath -> IO ()
runScript path = do
    code <- readFile path `catch` errorHandler path
    state <- defaultInterpreterState path

    (result, _) <- interpret state code
    case result of
        Left msg    -> do
            hPutStrLn stderr msg
            exitFailure
        Right _     -> return ()

  where
    errorHandler :: String -> IOError -> IO Text
    errorHandler _ err = do
        hPutStrLn stderr $ "Unable to open '" ++ path ++ "': " ++ ioeGetErrorString err
        exitFailure
