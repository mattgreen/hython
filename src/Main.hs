module Main (main)
where

import Control.Exception
import Control.Monad
import Control.Monad.State

import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Text.Printf

import Language.Python.Parser

import Hython.Monad
import Hython.Interpreter
import Hython.Object

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
            forM_ results $ \r -> liftIO $ putStrLn $ toStr r
            return ()

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

toStr :: Object -> String
toStr (None) = "None"
toStr (Bool b) = if b then "True" else "False"
toStr (Bytes _b) = "b'??'"
toStr (Float f) = show f
toStr (Imaginary i) = show i
toStr (Int i) = show i
toStr (String s) = "'" ++ s ++ "'"
toStr (BuiltinFn name)  = "<built-in function " ++ name ++ ">"
