import Control.Monad
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

import AST
import Parser

type Env = IORef (Map String Expression)

nullEnv :: IO Env
nullEnv = newIORef $ Map.fromList [("None", None), ("True", Bool True), ("False", Bool False)]

eval :: Env -> Statement -> IO ()
eval env (Block statements) = mapM_ (eval env) statements

eval envRef (Assignment var expr) = do
    value <- evalExpr envRef expr
    env <- readIORef envRef
    writeIORef envRef (Map.insert var value env)

eval env (If condition statement) = do
    result <- evalExpr env condition
    when (isTruthy result) (eval env statement)
    return ()

eval env (Expression e) = do
    _ <- evalExpr env e
    return ()

evalExpr :: Env -> Expression -> IO Expression
evalExpr _ (BinOp Add (Int l) (Int r)) = return $ Int (l + r)
evalExpr _ (BinOp Add (String l) (String r)) = return $ String (l ++ r)
evalExpr _ (BinOp Sub (Int l) (Int r)) = return $ Int (l - r)
evalExpr _ (BinOp Mul (Int l) (Int r)) = return $ Int (l * r)
evalExpr _ (BinOp Div (Int l) (Int r)) = return $ Int (quot l r)
evalExpr _ (BinOp Eq (Int l) (Int r)) = return $ Bool (l == r)
evalExpr _ (BinOp Eq (String l) (String r)) = return $ Bool (l == r)
evalExpr _ (BinOp Eq (Bool l) (Bool r)) = return $ Bool (l == r)
evalExpr _ (BinOp NotEq (Int l) (Int r)) = return $ Bool (l /= r)
evalExpr _ (BinOp NotEq (String l) (String r)) = return $ Bool (l /= r)
evalExpr _ (BinOp NotEq (Bool l) (Bool r)) = return $ Bool (l /= r)

evalExpr env (BinOp op l r) = do
    left <- evalExpr env l
    right <- evalExpr env r
    evalExpr env $ BinOp op left right

evalExpr env (Call _ expr) = do
    arg <- liftM toString (evalExpr env expr)
    putStrLn arg
    return None

evalExpr envRef (Variable var) = do
    env <- readIORef envRef
    case Map.lookup var env of
        Nothing -> fail $ "unknown variable " ++ var
        Just v  -> return v

evalExpr _ (Int n) = return $ Int n
evalExpr _ (String s) = return $ String s
evalExpr _ (Bool b) = return $ Bool b
evalExpr _ (None) = return None
evalExpr _ e = fail $ "Unimplemented: " ++ show e

isTruthy :: Expression -> Bool
isTruthy (Int 0) = False
isTruthy (Bool False) = False
isTruthy (None) = False
isTruthy _ = True

toString :: Expression -> String
toString (String v) = v
toString (Int v) = show v
toString e = show e

parseEval :: String -> String -> IO ()
parseEval filename code = do
    env <- nullEnv
    case parse filename code of
        Left e -> print e
        Right r -> mapM_ (eval env) r
    return ()

main :: IO ()
main = do
    [filename] <- getArgs
    code <- readFile filename

    parseEval filename code
