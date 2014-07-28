import Control.Monad
import Control.Monad.State
import Data.Complex
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment
import Text.Printf

import AST
import Parser

type SymbolTable = Map String Value
data Flow = Next | Breaking | Continuing | Returning Value deriving (Eq, Show)

data Environment = Environment {
    globals :: SymbolTable,
    frames :: [SymbolTable],
    flow :: Flow,
    loopLevel :: Int
} deriving (Show)

defaultEnv :: Environment
defaultEnv = Environment {globals = Map.empty, frames = [], flow = Next, loopLevel = 0}

lookupSymbol :: String -> StateT Environment IO Value
lookupSymbol name = do
    env <- get
    let symbols = if List.null (frames env)
                      then globals env
                      else head $ frames env

    case Map.lookup name symbols of
        Nothing -> fail $ "unknown symbol: " ++ name
        Just v  -> return v

updateSymbol :: String -> Value -> StateT Environment IO ()
updateSymbol name value = do
    env <- get
    if List.null (frames env)
        then put env { globals = Map.insert name value (globals env) }
        else do
            let frame = Map.insert name value (head $ frames env)
            let rest = tail $ frames env
            put env { frames = frame : rest }

eval :: Statement -> StateT Environment IO ()
eval (Def name params body) = do
    env <- get
    let symbols = globals env
    put env { globals = Map.insert name (Function name params body) symbols }

eval (Assignment var expr) = do
    value <- evalExpr expr
    updateSymbol var value

eval (Break) = do
    env <- get
    when (loopLevel env <= 0) (fail "Can only break in a loop!")
    put env { flow = Breaking }

eval (Continue) = do
    env <- get
    when (loopLevel env <= 0) (fail "Can only continue in a loop!")
    put env { flow = Continuing }

eval (If clauses elseBlock) = do
    evalClauses clauses
    return ()

    where
          evalClauses [] = evalBlock elseBlock
          evalClauses (IfClause condition block : rest) = do
            result <- evalExpr condition
            if isTruthy result
                then evalBlock block
                else evalClauses rest

eval (Return expression) = do
    value <- evalExpr expression
    env <- get
    put env { flow = Returning value }

    return ()

eval (While condition block) = do
    setup
    loop
    cleanup

    where
        setup = do
            env <- get
            let level = loopLevel env
            put env { loopLevel = level + 1 }

        loop = do
            env <- get
            result <- evalExpr condition
            when (isTruthy result && flow env == Next) $ do
                evalBlock block

                -- Pretty ugly! Eat continue.
                updatedEnv <- get
                when (flow updatedEnv == Continuing) $ put updatedEnv { flow = Next }

                loop

        cleanup = do
            env <- get
            let level = loopLevel env
            put env { loopLevel = level - 1 }

            case flow env of
                Breaking    -> put env { flow = Next }
                Continuing  -> put env { flow = Next }
                _           -> return ()

eval (Pass) = return ()

eval (Assert e) = do
    result <- evalExpr e
    unless (isTruthy result) (fail "Assertion failed!")

eval (Expression e) = do
    _ <- evalExpr e
    return ()

evalExpr :: Expression -> StateT Environment IO Value
evalExpr (BinOp (ArithOp op) (Constant (Int l)) (Constant (Int r)))
    | op == Add = return $ Int (l + r)
    | op == Sub = return $ Int (l - r)
    | op == Mul = return $ Int (l * r)
    | op == Div = return $ Float (fromInteger l / fromInteger r)

evalExpr (BinOp (ArithOp op) (Constant (Float l)) (Constant (Float r))) =
    return $ Float (fn op l r)
  where
    fn Add = (+)
    fn Sub = (-)
    fn Mul = (*)
    fn Div = (/)

-- Promote ints to floats in binary operators
evalExpr (BinOp op (Constant (Int l)) (Constant (Float r))) =
    evalExpr $ BinOp op (Constant (Float (fromIntegral l))) (Constant (Float r))

evalExpr (BinOp op (Constant (Float l)) (Constant (Int r))) =
    evalExpr $ BinOp op (Constant (Float l)) (Constant (Float (fromIntegral r)))

evalExpr (BinOp (ArithOp Add) (Constant (String l)) (Constant (String r))) =
    return $ String (l ++ r)

evalExpr (BinOp (ArithOp Mul) (Constant (Int l)) (Constant (String r))) =
    return $ String (concat $ replicate (fromInteger l) r)

evalExpr (BinOp (ArithOp Mul) (Constant (String l)) (Constant (Int r))) =
    return $ String (concat $ replicate (fromInteger r) l)

evalExpr (BinOp (BoolOp op) (Constant (Int l)) (Constant (Int r))) =
    return $ Bool (fn op l r)
  where
    fn Eq               = (==)
    fn NotEq            = (/=)
    fn LessThan         = (<)
    fn LessThanEq       = (<=)
    fn GreaterThan      = (>)
    fn GreaterThanEq    = (>=)

evalExpr (BinOp (BoolOp op) (Constant (Float l)) (Constant (Float r))) =
    return $ Bool (fn op l r)
  where
    fn Eq               = (==)
    fn NotEq            = (/=)
    fn LessThan         = (<)
    fn LessThanEq       = (<=)
    fn GreaterThan      = (>)
    fn GreaterThanEq    = (>=)

evalExpr (BinOp (BoolOp Eq) (Constant l) (Constant r)) =
    return $ Bool (l == r)

evalExpr (BinOp (BoolOp NotEq) (Constant l) (Constant r)) =
    return $ Bool (l /= r)

evalExpr (BinOp op (Constant l) (Constant r)) =
    fail $ printf "Unsupported operand type(s) for %s: %s %s" (show op) (toString l) (toString r)

evalExpr (BinOp op l r) = do
    left <- evalExpr l
    right <- evalExpr r
    evalExpr $ BinOp op (Constant left) (Constant right)

evalExpr (Call "print" args) = do
    arg <- liftM toString (evalExpr (head args))
    liftIO $ putStrLn arg
    return None

evalExpr (Call name args) = do
    f <- lookupSymbol name
    evalArgs <- mapM evalExpr args
    evalCall f evalArgs

evalExpr (Variable var) = lookupSymbol var
evalExpr (Constant c) = return c

evalBlock :: [Statement] -> StateT Environment IO ()
evalBlock statements = do
    env <- get

    case flow env of
        Next -> case statements of
            (s:r) -> do
                eval s
                evalBlock r
            [] -> return ()
        Continuing -> return ()
        _ -> return ()

evalCall :: Value -> [Value] -> StateT Environment IO Value
evalCall (Function _ params body) args = do
    env <- get
    let level = loopLevel env

    setup
    evalBlock body
    result <- returnValue
    teardown level
    return result

    where
        setup = do
            env <- get
            let frame = Map.union (Map.fromList $ zip params args) (globals env)
            put env { frames = frame : frames env, loopLevel = 0 }

        returnValue = do
            env <- get
            case flow env of
                Returning v -> return v
                _           -> return None

        teardown level = do
            env <- get
            put env { frames = tail (frames env), flow = Next, loopLevel = level }

evalCall v _ = fail $ "Unable to call " ++ show v

isTruthy :: Value -> Bool
isTruthy (Int 0) = False
isTruthy (Bool False) = False
isTruthy (None) = False
isTruthy _ = True

toString :: Value -> String
toString (None) = "None"
toString (Bool v) = show v
toString (String v) = v
toString (Int v) = show v
toString (Float v) = show v
toString (Imaginary v)
    | realPart v == 0   = show (imagPart v) ++ "j"
    | otherwise         = show v
toString (Function name _ _) = printf "<%s>" name

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
