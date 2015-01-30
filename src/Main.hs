module Main (main)
where

import Control.Exception

import System.Environment
import System.Exit
import System.IO.Error
import Text.Printf

import Hython.Interpreter

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename]  -> do
            code <- readFile filename `catch` errorHandler filename
            interpret filename code

        _           -> repl

errorHandler :: String -> IOError -> IO String
errorHandler filename e = do
    putStrLn $ printf "Unable to open '%s': file %s" filename (ioeGetErrorString e)
    _ <- exitFailure
    return ""
