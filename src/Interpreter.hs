import Control.Monad
import Control.Monad.State
import Data.Complex
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment
import Text.Printf

import Language
import Parser

type Evaluator = StateT Environment IO
type SymbolTable = Map String Value
data Flow = Next | Breaking | Continuing | Returning Value deriving (Eq, Show)

data Environment = Environment {
    scopes :: [SymbolTable],
    flow :: Flow,
    loopLevel :: Int
} deriving (Show)

defaultEnv :: Environment
defaultEnv = do
    let builtins = []
    let globals = Map.fromList builtins
    Environment {scopes = [globals], flow = Next, loopLevel = 0}

currentScope :: Evaluator SymbolTable
currentScope = do
    env <- get
    return $ head $ scopes env

getAttr :: String -> Value -> Evaluator (Maybe Value)
getAttr attr (Object _ ref) = do
    dict <- liftIO $ readIORef ref
    return $ Map.lookup attr dict
getAttr attr (Class _ ref) = do
    dict <- liftIO $ readIORef ref
    return $ Map.lookup attr dict
getAttr _ _ = fail "Only classes and objects have attrs!"

getClassAttr :: String -> Value -> Evaluator (Maybe Value)
getClassAttr attr (Object cls _) = getAttr attr cls
getClassAttr _ _ = fail "Only objects have class attrs!"

setAttr :: String -> Value -> Value -> Evaluator ()
setAttr attr value (Object _ ref)   = liftIO $ modifyIORef ref (Map.insert attr value)
setAttr attr value (Class _ ref)    = liftIO $ modifyIORef ref (Map.insert attr value)
setAttr _ _ _                       = fail "Only objects have attrs!"

lookupSymbol :: String -> Evaluator Value
lookupSymbol name = do
    scope <- currentScope
    case Map.lookup name scope of
        Nothing -> fail $ "Unknown symbol: " ++ name
        Just v  -> return v

updateSymbol :: String -> Value -> Evaluator ()
updateSymbol name value = do
    scope <- currentScope
    env <- get

    let updatedScope = Map.insert name value scope
    put env { scopes = updatedScope : tail (scopes env) }

eval :: Statement -> Evaluator ()
eval (Def name params body) = updateSymbol name $ Function name params body

eval (ClassDef name _ statements) = do
    pushScope
    mapM_ eval statements
    dict <- popScope

    updateSymbol name $ Class name dict

  where
    pushScope = do
        let dict = Map.empty
        env <- get
        put env { scopes = dict : scopes env }

    popScope = do
        env <- get
        let dict = head $ scopes env
        put env { scopes = tail $ scopes env }
        liftIO $ newIORef dict

eval (Assignment (Variable var) expr) = do
    value <- evalExpr expr
    updateSymbol var value

eval (Assignment (Attribute var attr) expr) = do
    value <- evalExpr expr
    obj <- lookupSymbol var
    setAttr attr value obj

eval (Assignment{}) = fail "Syntax error!"

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

evalExpr :: Expression -> Evaluator Value
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

-- String + String
evalExpr (BinOp (ArithOp Add) (Constant (String l)) (Constant (String r))) =
    return $ String (l ++ r)

-- Int * String
evalExpr (BinOp (ArithOp Mul) (Constant (Int l)) (Constant (String r))) =
    return $ String (concat $ replicate (fromInteger l) r)

-- String * Int
evalExpr (BinOp (ArithOp Mul) (Constant (String l)) (Constant (Int r))) =
    return $ String (concat $ replicate (fromInteger r) l)

-- Int [=|!=|<|<=|>|>=] Int
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

evalExpr (MethodCall target name args) = do
    receiver <- lookupSymbol target
    evalArgs <- mapM evalExpr args
    method <- getClassAttr name receiver

    case method of
        Just f  -> evalCall f (receiver : evalArgs)
        Nothing -> fail $ "Unknown method " ++ name

evalExpr (Attribute target name) = do
    receiver <- lookupSymbol target
    attribute <- getAttr name receiver
    case attribute of
        Just v  -> return v
        Nothing -> fail $ "No attribute " ++ name

evalExpr (Variable var) = lookupSymbol var
evalExpr (Constant c) = return c

evalBlock :: [Statement] -> Evaluator ()
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

evalCall :: Value -> [Value] -> Evaluator Value
evalCall cls@(Class {}) args = do
    dict <- liftIO $ newIORef (Map.fromList [("__class__", cls)])
    let obj = Object cls dict
    ctor <- getAttr "__init__" cls

    _ <- case ctor of
        Just f  -> evalCall f (obj : args)
        Nothing -> return None

    return obj

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
            let scope = Map.union (Map.fromList $ zip params args) (last $ scopes env)
            put env { scopes = scope : scopes env, loopLevel = 0 }

        returnValue = do
            env <- get
            case flow env of
                Returning v -> return v
                _           -> return None

        teardown level = do
            env <- get
            put env { scopes = tail (scopes env), flow = Next, loopLevel = level }

evalCall v _ = fail $ "Unable to call " ++ toString v

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
toString (Class name _) = printf "<class '__main__.%s'>" name
toString (Object (Class name _) _) = printf "<%s object>" name
toString (Object _ _) = fail "Object must be associated with a class!"

parseEval :: String -> String -> Evaluator ()
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
