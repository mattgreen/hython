import Control.Monad
import Control.Monad.State
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment

import AST
import Parser

type SymbolTable = Map String Value
data Flow = Continue | Breaking | Returning Value deriving (Eq, Show)

data Environment = Environment {
    globals :: SymbolTable,
    frames :: [SymbolTable],
    flow :: Flow,
    loopLevel :: Int
} deriving (Show)

defaultEnv :: Environment
defaultEnv = do
    let builtins = Map.fromList [("None", None), ("True", Bool True), ("False", Bool False)]
    Environment {globals = builtins, frames = [], flow = Continue, loopLevel = 0}

eval :: Statement -> StateT Environment IO ()
eval (Def name params body) = do
    env <- get
    let symbols = globals env
    put env { globals = Map.insert name (Function params body) symbols }

eval (Assignment var expr) = do
    value <- evalExpr expr
    env <- get
    let symbols = globals env
    put env { globals = Map.insert var value symbols }

eval (Break) = do
    env <- get
    when (loopLevel env <= 0) (fail "Can only break in a loop!")
    put env { flow = Breaking }

eval (If condition thenBlock elseBlock) = do
    result <- evalExpr condition
    _ <- evalBlock (if isTruthy result then thenBlock else elseBlock)
    return ()

eval (Return expression) = do
    value <- evalExpr expression

    env <- get
    put env { flow = Returning value }

    return ()

eval (While condition block) = do
    env <- get
    result <- evalExpr condition
    let level = loopLevel env

    put env { loopLevel = level + 1 }
    when (isTruthy result) $ do
        continue <- evalBlock block
        when continue (eval $ While condition block)
    put env { flow = Continue, loopLevel = level - 1 }

    return ()

eval (Expression e) = do
    _ <- evalExpr e
    return ()

evalExpr :: Expression -> StateT Environment IO Value
evalExpr (BinOp Add (Constant (Int l)) (Constant (Int r))) = return $ Int (l + r)
evalExpr (BinOp Add (Constant (String l)) (Constant (String r))) = return $ String (l ++ r)
evalExpr (BinOp Sub (Constant (Int l)) (Constant (Int r))) = return $ Int (l - r)
evalExpr (BinOp Mul (Constant (Int l)) (Constant (Int r))) = return $ Int (l * r)
evalExpr (BinOp Div (Constant (Int l)) (Constant (Int r))) = return $ Int (quot l r)
evalExpr (BinOp Eq (Constant (Int l)) (Constant (Int r))) = return $ Bool (l == r)
evalExpr (BinOp Eq (Constant (String l)) (Constant (String r))) = return $ Bool (l == r)
evalExpr (BinOp Eq (Constant (Bool l)) (Constant (Bool r))) = return $ Bool (l == r)
evalExpr (BinOp NotEq (Constant (Int l)) (Constant (Int r))) = return $ Bool (l /= r)
evalExpr (BinOp NotEq (Constant (String l)) (Constant (String r))) = return $ Bool (l /= r)
evalExpr (BinOp NotEq (Constant (Bool l)) (Constant (Bool r))) = return $ Bool (l /= r)

evalExpr (BinOp op l r) = do
    left <- evalExpr l
    right <- evalExpr r
    evalExpr $ BinOp op (Constant left) (Constant right)

evalExpr (Call "print" args) = do
    arg <- liftM toString (evalExpr (head args))
    liftIO $ putStrLn arg
    return None

evalExpr (Call name args) = do
    env <- get
    let symbols = globals env

    evalArgs <- mapM evalExpr args

    case Map.lookup name symbols of
        Nothing -> fail $ "unknown function: " ++ name
        Just f -> evalCall f evalArgs

evalExpr (Variable var) = do
    env <- get
    let symbols = if List.null (frames env)
                      then globals env
                      else head $ frames env

    case Map.lookup var symbols of
        Nothing -> fail $ "unknown variable " ++ var
        Just v  -> return v

evalExpr (Constant c) = return c

evalBlock :: [Statement] -> StateT Environment IO Bool
evalBlock statements = do
    env <- get

    case flow env of
        Continue -> case statements of
            (s:r) -> do
                eval s
                evalBlock r
            [] -> return True
        _ -> return False

evalCall :: Value -> [Value] -> StateT Environment IO Value
evalCall (Function params body) args = do
    setup
    _ <- evalBlock body
    result <- returnValue
    teardown
    return result

    where
        setup = do
            env <- get
            let frame = Map.union (Map.fromList $ zip params args) (globals env)
            put env { frames = frame : frames env }

        returnValue = do
            env <- get
            case flow env of
                Returning v -> return v
                _           -> return None

        teardown = do
            env <- get
            put env { frames = tail (frames env), flow = Continue }
evalCall v _ = fail $ "Unable to call " ++ show v

isTruthy :: Value -> Bool
isTruthy (Int 0) = False
isTruthy (Bool False) = False
isTruthy (None) = False
isTruthy _ = True

toString :: Value -> String
toString (String v) = v
toString (Int v) = show v
toString e = show e

parseEval :: String -> String -> StateT Environment IO ()
parseEval filename code = do
    case parse filename code of
        Left e -> liftIO $ print e
        Right r -> mapM_ eval r
    return ()

main :: IO ()
main = do
    [filename] <- getArgs
    code <- readFile filename

    _ <- runStateT (parseEval filename code) defaultEnv
    return ()
