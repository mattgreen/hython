import System.Environment
import Text.ParserCombinators.Parsec

data Statement = Print String
    deriving(Show)

statement = do
    string "print(\""
    content <- many (noneOf "\"")
    string "\")"
    spaces
    return $ Print content

statements = many statement

eval :: Statement -> IO ()
eval (Print m) = print m

main = do
    [filename] <- getArgs
    code <- readFile filename

    case parse statements filename code of
        Left e  -> print e
        Right r -> mapM_ eval r
