import Control.Monad
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

import AST
import Parser

type Env = IORef (Map String Value)

nullEnv :: IO Env
nullEnv = newIORef $ Map.fromList [("None", None), ("True", Bool True), ("False", Bool False)]

eval :: Env -> Statement -> IO ()
eval envRef (Def name params body) = do
    env <- readIORef envRef
    writeIORef envRef (Map.insert name (Function params body) env)

eval envRef (Assignment var expr) = do
    value <- evalExpr envRef expr
    env <- readIORef envRef
    writeIORef envRef (Map.insert var value env)

eval env (If condition statements) = do
    result <- evalExpr env condition
    when (isTruthy result) (mapM_ (eval env) statements)
    return ()

eval env (Expression e) = do
    _ <- evalExpr env e
    return ()

evalExpr :: Env -> Expression -> IO Value
evalExpr _ (BinOp Add (Constant (Int l)) (Constant (Int r))) = return $ Int (l + r)
evalExpr _ (BinOp Add (Constant (String l)) (Constant (String r))) = return $ String (l ++ r)
evalExpr _ (BinOp Sub (Constant (Int l)) (Constant (Int r))) = return $ Int (l - r)
evalExpr _ (BinOp Mul (Constant (Int l)) (Constant (Int r))) = return $ Int (l * r)
evalExpr _ (BinOp Div (Constant (Int l)) (Constant (Int r))) = return $ Int (quot l r)
evalExpr _ (BinOp Eq (Constant (Int l)) (Constant (Int r))) = return $ Bool (l == r)
evalExpr _ (BinOp Eq (Constant (String l)) (Constant (String r))) = return $ Bool (l == r)
evalExpr _ (BinOp Eq (Constant (Bool l)) (Constant (Bool r))) = return $ Bool (l == r)
evalExpr _ (BinOp NotEq (Constant (Int l)) (Constant (Int r))) = return $ Bool (l /= r)
evalExpr _ (BinOp NotEq (Constant (String l)) (Constant (String r))) = return $ Bool (l /= r)
evalExpr _ (BinOp NotEq (Constant (Bool l)) (Constant (Bool r))) = return $ Bool (l /= r)

evalExpr env (BinOp op l r) = do
    left <- evalExpr env l
    right <- evalExpr env r
    evalExpr env $ BinOp op (Constant left) (Constant right)

evalExpr env (Call "print" args) = do
    arg <- liftM toString (evalExpr env (head args))
    putStrLn arg
    return None

evalExpr envRef (Call name args) = do
    env <- readIORef envRef
    case Map.lookup name env of
        Nothing -> fail $ "unknown function: " ++ name
        Just f -> evalCall envRef f args

evalExpr envRef (Variable var) = do
    env <- readIORef envRef
    case Map.lookup var env of
        Nothing -> fail $ "unknown variable " ++ var
        Just v  -> return v

evalExpr _ (Constant c) = return c

evalCall env (Function _ body) args = do
    mapM_ (eval env) body
    return None

isTruthy :: Value -> Bool
isTruthy (Int 0) = False
isTruthy (Bool False) = False
isTruthy (None) = False
isTruthy _ = True

toString :: Value -> String
toString (String v) = v
toString (Int v) = show v
toString e = show e

parseEval :: String -> String -> IO ()
parseEval filename code = do
    env <- nullEnv
    case parse filename code of
        Left e -> print e
        {-Right r -> print r-}
        Right r -> mapM_ (eval env) r
    return ()

main :: IO ()
main = do
    [filename] <- getArgs
    code <- readFile filename

    parseEval filename code
