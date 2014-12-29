import System.Environment
import System.Exit

import Hython.Interpreter

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename]  -> do
            code <- readFile filename
            interpret filename code

        _           -> do
            putStrLn "Usage: hython <filename>"
            exitFailure
