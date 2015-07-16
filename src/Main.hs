module Main (main)
where

import Control.Exception
import Control.Monad

import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Text.Printf

import Language.Python.Parser (parse)

import Hython.Builtins (toStr)
import Hython.Interpreter (runInterpreter)
import Hython.Monad (evalBlock, liftIO)

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
runREPL = runInterpreter $ forever $ do
    line <- liftIO $ do
        putStr ">>> "
        hFlush stdout
        getLine

    case parse line of
        Left msg    -> liftIO $ putStrLn msg
        Right stmts -> do
            results <- evalBlock stmts
            forM_ results $ liftIO . putStrLn . toStr

runScript :: String -> IO ()
runScript filename = do
    code <- readFile filename `catch` errorHandler filename

    runInterpreter $ case parse code of
        Left msg    -> error msg
        Right statements -> do
            _ <- evalBlock statements
            return ()

  where
    errorHandler :: String -> IOError -> IO String
    errorHandler _ err = do
        putStrLn $ printf "Unable to open '%s': file %s" filename (ioeGetErrorString err)
        _ <- exitFailure
        return ""
