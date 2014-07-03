import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec

data Expression =
      Print Expression
    | StringLiteral String
    | None
    deriving(Show)

printExpr = do
    string "print"
    string "("
    expr <- expression
    string ")"
    spaces
    return $ Print expr

literal = do
    char '"'
    content <- many (noneOf "\"")
    char '"'
    return $ StringLiteral content

expression = printExpr <|> literal
expressions = many expression

toString :: Expression -> String
toString (StringLiteral v) = v

eval :: Expression -> IO Expression
eval (Print e) = do
    arg <- liftM toString (eval e)
    putStrLn arg
    return None
eval e = return e


main = do
    [filename] <- getArgs
    code <- readFile filename

    case parse expressions filename code of
        Left e  -> print e
        Right r -> mapM_ eval r
