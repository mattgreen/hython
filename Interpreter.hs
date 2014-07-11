import Control.Monad
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

import AST
import Parser

type EnvOld = IORef[(String, IORef Expression)]
type Env = IORef (Map String Expression)

eval :: Env -> Expression -> IO Expression

eval env (Block expressions) = do
    mapM_ (eval env) expressions
    return None

eval env (Call fn expr) = do
    arg <- liftM toString (eval env expr)
    putStrLn arg
    return None

eval envRef (Assignment var expr) = do
    value <- eval envRef expr
    env <- readIORef envRef
    writeIORef envRef (Map.insert var value env)

    return value

eval envRef (LocalVar var) = do
    env <- readIORef envRef
    case (Map.lookup var env) of
        Nothing -> fail $ "unknown variable " ++ var
        Just v  -> return v
    {-value <- maybe (fail $ "unknown variable " ++ var) (id) (Map.lookup var env)-}
    {-return value-}

eval env (BinOp (Add) (Int l) (Int r)) = return $ Int (l + r)
eval env (BinOp (Add) (String l) (String r)) = return $ String (l ++ r)
eval env (BinOp (Sub) (Int l) (Int r)) = return $ Int (l - r)
eval env (BinOp (Mul) (Int l) (Int r)) = return $ Int (l * r)
eval env (BinOp (Div) (Int l) (Int r)) = return $ Int (quot l r)

eval env (BinOp op l r) = do
    left <- eval env l
    right <- eval env r
    result <- eval env $ BinOp op left right
    return result

eval env (Int n) = return $ Int n
eval env (String s) = return $ String s

toString :: Expression -> String
toString (String v) = v
toString (Int v) = show v
toString e = show e

parseEval :: String -> IO ()
parseEval code = do
    env <- newIORef Map.empty
    case parseCode code of
        Left e -> print e
        Right r -> mapM_ (eval env) r
    return ()

main = do
    [filename] <- getArgs
    code <- readFile filename

    parseEval code
