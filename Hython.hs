import Control.Monad
import System.Environment

import AST
import Parser

eval :: Expression -> IO Expression
eval (Call fn expr) = do
    arg <- liftM toString (eval expr)
    putStrLn arg
    return None

eval (BinOp (Add) (Int l) (Int r)) = return $ Int (l + r)
eval (BinOp (Add) (String l) (String r)) = return $ String (l ++ r)
eval (BinOp (Sub) (Int l) (Int r)) = return $ Int (l - r)
eval (BinOp (Mul) (Int l) (Int r)) = return $ Int (l * r)
eval (BinOp (Div) (Int l) (Int r)) = return $ Int (quot l r)

eval (BinOp op l r) = do
    left <- eval l
    right <- eval r
    result <- eval $ BinOp op left right
    return result

eval (Int n) = return $ Int n
eval (String s) = return $ String s

toString :: Expression -> String
toString (String v) = v
toString (Int v) = show v
toString e = show e

parseEval :: String -> IO ()
parseEval code = case parseCode code of
    Left e  -> print e
    Right r -> mapM_ eval r

main = do
    [filename] <- getArgs
    code <- readFile filename

    parseEval code
