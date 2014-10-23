import System.Environment

import Interpreter

main :: IO ()
main = do
    [filename]  <- getArgs
    code        <- readFile filename

    interpret filename code
