module Main (main)
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Text.Printf

import Language.Python.Parser (parse)

import Hython.Builtins (toStr)
import Hython.Interpreter (Interpreter, runInterpreter)
import Hython.Object (evalBlock, Object, raiseError)

main :: IO ()
main = do
    args <- getArgs
    case args of
        []          -> runREPL
        [filename]  -> runScript filename
        _           -> do
            putStrLn "Usage: hython <filename>"
            exitFailure

parseEval :: String -> Interpreter [Object]
parseEval code = case parse code of
    Left msg    -> raiseError "SyntaxError" msg >> return []
    Right stmts -> evalBlock stmts

runREPL :: IO ()
runREPL = runInterpreter $ forever $ do
    line <- liftIO $ do
        putStr ">>> "
        hFlush stdout
        getLine `catch` errorHandler

    results <- parseEval line
    mapM_ (liftIO . putStrLn . toStr) results
  where
    errorHandler :: IOError -> IO String
    errorHandler _ = putStrLn "" >> exitSuccess

runScript :: String -> IO ()
runScript filename = do
    code <- readFile filename `catch` errorHandler filename

    runInterpreter $ do
        _ <- parseEval code
        return ()

  where
    errorHandler :: String -> IOError -> IO String
    errorHandler _ err = do
        putStrLn $ printf "Unable to open '%s': file %s" filename (ioeGetErrorString err)
        _ <- exitFailure
        return ""
