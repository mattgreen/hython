import System.Environment

import Hython.Interpreter

main :: IO ()
main = do
    [filename]  <- getArgs
    code        <- readFile filename

    interpret filename code
