import Control.Monad
import Data.IORef
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

import AST
import Parser

type SymbolTable = Map String Value
data ReturnValue = Unset | Value Value deriving (Eq, Show)

data EnvironmentState = Environment {
    symbolTable :: SymbolTable,
    frames :: [SymbolTable],
    returnValue :: ReturnValue
} deriving (Show)

type Environment = IORef EnvironmentState

defaultEnv :: IO Environment
defaultEnv = do
    let builtins = Map.fromList [("None", None), ("True", Bool True), ("False", Bool False)]
    newIORef Environment {symbolTable = builtins, frames = [], returnValue = Unset }

eval :: Environment -> Statement -> IO ()
eval envRef (Def name params body) = do
    env <- readIORef envRef
    let symbols = symbolTable env
    writeIORef envRef env { symbolTable = Map.insert name (Function params body) symbols }

eval envRef (Assignment var expr) = do
    value <- evalExpr envRef expr
    env <- readIORef envRef
    let symbols = symbolTable env
    writeIORef envRef env { symbolTable = Map.insert var value symbols }

eval env (If condition thenBlock elseBlock) = do
    result <- evalExpr env condition
    evalBlock (if isTruthy result then thenBlock else elseBlock)
    where evalBlock = mapM_ $ eval env

eval envRef (Return expression) = do
    value <- evalExpr envRef expression

    env <- readIORef envRef
    writeIORef envRef env { returnValue = Value value }

    return ()

eval env (Expression e) = do
    _ <- evalExpr env e
    return ()

evalExpr :: Environment -> Expression -> IO Value
evalExpr _ (BinOp Add (Constant (Int l)) (Constant (Int r))) = return $ Int (l + r)
evalExpr _ (BinOp Add (Constant (String l)) (Constant (String r))) = return $ String (l ++ r)
evalExpr _ (BinOp Sub (Constant (Int l)) (Constant (Int r))) = return $ Int (l - r)
evalExpr _ (BinOp Mul (Constant (Int l)) (Constant (Int r))) = return $ Int (l * r)
evalExpr _ (BinOp Mul (Constant (String l)) (Constant (Int r))) = return $ String (foldl (\(t,_) -> t ++ s) "" [1..n])
evalExpr _ (BinOp Mul (Constant (Int l)) (Constant (String r))) = return $ String (foldl (\(t,_) -> t ++ s) "" [1..n])
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
    let symbols = symbolTable env

    evalArgs <- mapM (evalExpr envRef) args

    case Map.lookup name symbols of
        Nothing -> fail $ "unknown function: " ++ name
        Just f -> evalCall envRef f evalArgs

evalExpr envRef (Variable var) = do
    env <- readIORef envRef
    let symbols = if List.null (frames env)
                      then symbolTable env
                      else head $ frames env

    case Map.lookup var symbols of
        Nothing -> fail $ "unknown variable " ++ var
        Just v  -> return v

evalExpr _ (Constant c) = return c

evalCall :: Environment -> Value -> [Value] -> IO Value
evalCall envRef (Function params body) args = do
    setup
    result <- evalBody body
    teardown
    return result

    where
        setup = do
            env <- readIORef envRef
            let frame = Map.union (Map.fromList $ zip params args) (symbolTable env)
            writeIORef envRef env { frames = frame : frames env }

        evalBody statements = do
            env <- readIORef envRef
            let earlyReturn = returnValue env

            case earlyReturn of
                Unset -> case statements of
                        (s:r) -> do
                            eval envRef s
                            evalBody r
                        [] -> return None
                Value v -> return v

        teardown = do
            env <- readIORef envRef
            writeIORef envRef env { frames = tail (frames env), returnValue = Unset }

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
    env <- defaultEnv
    case parse filename code of
        Left e -> print e
        Right r -> mapM_ (eval env) r
    return ()

main :: IO ()
main = do
    [filename] <- getArgs
    code <- readFile filename

    parseEval filename code
