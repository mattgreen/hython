{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (break)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont
import Data.Bits
import Data.Fixed
import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List hiding (break)
import Debug.Trace
import System.Environment
import Text.Printf

import Builtins (pow, str, builtinFunctions, BuiltInFunction)
import Language
import Parser

type Evaluator = ContT () (ReaderT FlowControl (StateT Environment IO))
type EvaluatorCont = () -> Evaluator ()
type EvaluatorReturnCont = Value -> Evaluator ()
type EvaluatorExceptCont = Value -> Evaluator ()
type SymbolTable = HashMap String Value

data Environment = Environment {
    exceptHandler :: EvaluatorExceptCont,
    frames :: [String],
    scopes :: [SymbolTable],
    builtins :: [(String, BuiltInFunction)],
    fnReturn :: EvaluatorReturnCont
}

-- Need to track:
-- handler continuation
-- frame depth

data FlowControl = FlowControl {
    loopBreak :: EvaluatorCont,
    loopContinue :: EvaluatorCont
}

unimplemented :: Statement -> Evaluator ()
unimplemented s = fail $ printf "Unimplemented: %s" (show s)

defaultEnv :: Environment
defaultEnv = Environment {
    exceptHandler = error "Must be in exception handler",
    builtins = builtinFunctions,
    frames = ["<module>"],
    scopes = [Map.fromList []],
    fnReturn = error "Must be in function!"
}

defaultFlowControl :: FlowControl
defaultFlowControl = FlowControl {
    loopBreak = error "Must be in loop",
    loopContinue = error "Must be in loop"
}

currentScope :: Evaluator SymbolTable
currentScope = do
    current <- gets scopes
    return $ head current

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
        Just v  -> return v
        Nothing -> do
            builtinFns <- gets builtins
            case lookup name builtinFns of
                Nothing -> fail $ "Unknown symbol : " ++ name
                Just _  -> return $ BuiltinFn name


updateSymbol :: String -> Value -> Evaluator ()
updateSymbol name value = do
    scope <- currentScope

    let updatedScope = Map.insert name value scope
    modify $ \env -> env { scopes = updatedScope : tail (scopes env) }

restoreHandler :: (MonadState Environment m) => EvaluatorExceptCont -> (t -> m b) -> t -> m b
restoreHandler previousHandler action value = do
    modify $ \e -> e { exceptHandler = previousHandler }
    action value

eval :: Statement -> Evaluator ()
eval (Def name params body) = updateSymbol name function
  where
    function = Function name params body

eval (ModuleDef statements) = evalBlock statements

eval (ClassDef name _ statements) = do
    pushScope
    evalBlock statements
    dict <- popScope

    updateSymbol name $ Class name dict

  where
    pushScope = do
        let dict = Map.empty
        modify $ \env -> env { scopes = dict : scopes env }

    popScope = do
        currentScopes <- gets scopes
        let dict = head currentScopes
        modify $ \e -> e{ scopes = tail currentScopes }
        liftIO $ newIORef dict

eval (Assignment (Name var) expr) = do
    value <- evalExpr expr
    updateSymbol var value

eval (Assignment (Attribute var attr) expr) = do
    value <- evalExpr expr
    target <- evalExpr var
    setAttr attr value target

eval (Assignment{}) = fail "Syntax error!"

eval (Break) = do
    break <- asks loopBreak
    break ()

eval (Continue) = do
    continue <- asks loopContinue
    continue ()

-- Needs EH to implement iterator protocol
eval s@(For {}) = unimplemented s

eval s@(Global {}) = unimplemented s

eval (If clauses elseBlock) = evalClauses clauses
  where
    evalClauses [] = evalBlock elseBlock
    evalClauses (IfClause condition block : rest) = do
        result <- evalExpr condition
        if isTrue result
            then evalBlock block
            else evalClauses rest

eval s@(Nonlocal {}) = unimplemented s

eval (Raise {}) = do
    handler <- gets exceptHandler
    handler (Int 42)

eval s@(Reraise {}) = unimplemented s

eval (Return expression) = do
    value <- evalExpr expression

    returnCont <- gets fnReturn
    returnCont value

eval (Try clauses block elseBlock finallyBlock) = do
    previousHandler <- gets exceptHandler

    exception <- callCC $ \handler -> do
        modify $ \e -> e { exceptHandler = handler }
        evalBlock block
        return None

    handled <- case exception of
        None -> do
            evalBlock elseBlock
            return True

        _ -> case getHandler exception of
            Just handlerBlock -> do
                evalBlock handlerBlock
                return True

            Nothing -> return False

    evalBlock finallyBlock

    unless handled $
        previousHandler exception

  where
    getHandler None = Nothing
    getHandler exception =
        case Data.List.find (handlerFor exception) clauses of
            Just (ExceptClause _ handlerBlock)  -> Just handlerBlock
            Just (CatchAllClause handlerBlock)  -> Just handlerBlock
            Nothing                             -> Nothing

    handlerFor _ (ExceptClause _ _) = False
    handlerFor _ (CatchAllClause _) = True

eval (While condition block elseBlock) = do
    handler <- gets exceptHandler

    callCC $ \break ->
        fix $ \loop -> do
            callCC $ \continue -> do
                let breakHandler = restoreHandler handler break
                let continueHandler = restoreHandler handler continue

                local (\f -> f{ loopBreak = breakHandler, loopContinue = continueHandler }) $ do
                    result <- evalExpr condition
                    unless (isTrue result) $ do
                        evalBlock elseBlock
                        break ()
                    evalBlock block
            loop

eval s@(With {}) = unimplemented s

eval (Pass) = return ()

eval (Assert e _) = do
    result <- evalExpr e
    unless (isTrue result) (fail "Assertion failed!")

eval s@(Del {}) = unimplemented s

eval (Expression e) = do
    _ <- evalExpr e
    return ()

evalExpr :: Expression -> Evaluator Value
evalExpr (As expr binding) = do
    value <- evalExpr expr

    case binding of
        Name n  -> updateSymbol n value
        _       -> fail "unhandled binding type"

    return value

evalExpr (UnaryOp Not (Constant (Bool v))) =
    return $ Bool (not v)

evalExpr (UnaryOp Pos (Constant v@(Int {}))) =
    return v

evalExpr (UnaryOp Pos (Constant v@(Float {}))) =
    return v

evalExpr (UnaryOp Neg (Constant (Int v))) =
    return $ Int (- v)

evalExpr (UnaryOp Neg (Constant (Float v))) =
    return $ Float (- v)

evalExpr (UnaryOp Complement (Constant (Int v))) =
    return $ Int (complement v)

evalExpr (UnaryOp op (Constant r)) = do
    strExpr <- liftIO $ str r
    fail $ printf "Unsupported operand type for %s: %s" (show op) strExpr

evalExpr (UnaryOp op expr) = do
    r <- evalExpr expr
    evalExpr $ UnaryOp op (Constant r)

evalExpr (BinOp (ArithOp op) (Constant (Int l)) (Constant (Int r)))
    | op == Add = return $ Int (l + r)
    | op == Sub = return $ Int (l - r)
    | op == Mul = return $ Int (l * r)
    | op == Div = return $ Float (fromInteger l / fromInteger r)
    | op == Mod = return $ Int (l `mod` r)
    | op == FDiv = return $ Int (floorInt (fromIntegral l / fromIntegral r))
    | op == Pow = liftIO $ pow [Int l, Int r]
  where
    floorInt = floor :: Double -> Integer

evalExpr (BinOp (BitOp BitAnd) (Constant (Int l)) (Constant (Int r))) =
    return $ Int (l .&. r)

evalExpr (BinOp (BitOp BitOr) (Constant (Int l)) (Constant (Int r))) =
    return $ Int (l .|. r)

evalExpr (BinOp (BitOp BitXor) (Constant (Int l)) (Constant (Int r))) =
    return $ Int (xor l r)

evalExpr (BinOp (BitOp LShift) (Constant (Int l)) (Constant (Int r))) =
    return $ Int (shiftL l (fromIntegral r))

evalExpr (BinOp (BitOp RShift) (Constant (Int l)) (Constant (Int r))) =
    return $ Int (shiftR l (fromIntegral r))

evalExpr (BinOp (ArithOp op) (Constant (Float l)) (Constant (Float r)))
    | op == Add = return $ Float (l + r)
    | op == Sub = return $ Float (l - r)
    | op == Mul = return $ Float (l * r)
    | op == Div = return $ Float (l / r)
    | op == Mod = return $ Float (l `mod'` r)
    | op == FDiv = return $ Float (fromInteger (floor (l / r)))
    | op == Pow = liftIO $ pow [Float l, Float r]

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
evalExpr (BinOp (CompOp op) (Constant (Int l)) (Constant (Int r))) =
    return $ Bool (fn op l r)
  where
    fn Eq               = (==)
    fn NotEq            = (/=)
    fn LessThan         = (<)
    fn LessThanEq       = (<=)
    fn GreaterThan      = (>)
    fn GreaterThanEq    = (>=)

evalExpr (BinOp (CompOp op) (Constant (Float l)) (Constant (Float r))) =
    return $ Bool (fn op l r)
  where
    fn Eq               = (==)
    fn NotEq            = (/=)
    fn LessThan         = (<)
    fn LessThanEq       = (<=)
    fn GreaterThan      = (>)
    fn GreaterThanEq    = (>=)

evalExpr (BinOp (CompOp Eq) (Constant l) (Constant r)) =
    return $ Bool (l == r)

evalExpr (BinOp (CompOp NotEq) (Constant l) (Constant r)) =
    return $ Bool (l /= r)

evalExpr (BinOp op (Constant l) (Constant r)) = do
    left <- liftIO $ str l
    right <- liftIO $ str r
    fail $ printf "Unsupported operand type(s) for %s: %s %s" (show op) left right

evalExpr (BinOp op l r) = do
    left <- evalExpr l
    right <- evalExpr r
    evalExpr $ BinOp op (Constant left) (Constant right)

evalExpr (Call (Attribute obj name) args) = do
    receiver <- evalExpr obj
    evalArgs <- mapM evalExpr args
    method <- getClassAttr name receiver

    case method of
        Just f  -> evalCall f (receiver: evalArgs)
        Nothing -> fail $ "Unknown method: " ++ name

evalExpr (Call e args) = do
    f <- evalExpr e
    evalArgs <- mapM evalExpr args
    evalCall f evalArgs

evalExpr e@(Lambda {}) = fail $ "Unimplemented: " ++ show e

evalExpr (Attribute target name) = do
    receiver <- evalExpr target
    attribute <- getAttr name receiver
    case attribute of
        Just v  -> return v
        Nothing -> fail $ "No attribute " ++ name

evalExpr (SliceDef startExpr stopExpr strideExpr) = do
    start <- evalExpr startExpr
    stop <- evalExpr stopExpr
    stride <- evalExpr strideExpr

    return $ Slice start stop stride

evalExpr (ListDef exprs) = do
    values <- mapM evalExpr exprs
    ref <- liftIO $ newIORef values
    return $ List ref

evalExpr (Subscript expr sub) = do
    left <- evalExpr expr
    index <- evalExpr sub
    evalSubscript left index

  where
    evalSubscript (List ref) (Int i) = do
        values <- liftIO $ readIORef ref
        return $ values !! fromIntegral i
    evalSubscript (List {}) _ = fail "list indicies must be integers"
    evalSubscript (Tuple values) (Int i) = return $ values !! fromIntegral i
    evalSubscript (Tuple {}) _ = fail "tuple indicies must be integers"
    evalSubscript (String s) (Int i) = return $ String [s !! fromIntegral i]
    evalSubscript _ _ = fail "invalid subscript"

evalExpr (TernOp condExpr thenExpr elseExpr) = do
    condition <- evalExpr condExpr
    evalExpr $ if isTrue condition
        then thenExpr
        else elseExpr

evalExpr (TupleDef exprs) = do
    values <- mapM evalExpr exprs
    return $ Tuple values

evalExpr (From {}) = fail "Unimplemented: from"
evalExpr (Yield {}) = fail "Unimplemented: yield"

evalExpr (Name var) = lookupSymbol var
evalExpr (Constant c) = return c

evalBlock :: [Statement] -> Evaluator ()
evalBlock = mapM_ traceEval
  where
    traceEval :: Statement -> Evaluator ()
    traceEval s = do
        tracing <- liftIO $ lookupEnv "TRACE"

        case tracing of
            Just _  -> (trace $ traceStmt s) eval s
            Nothing -> eval s

    traceStmt s = "*** Evaluating: " ++ show s

evalCall :: Value -> [Value] -> Evaluator Value
evalCall cls@(Class {}) args = do
    dict <- liftIO $ newIORef (Map.fromList [("__class__", cls)])
    let obj = Object cls dict
    ctor <- getAttr "__init__" cls

    _ <- case ctor of
        Just f  -> evalCall f (obj : args)
        Nothing -> return None

    return obj

evalCall (BuiltinFn name) args = do
    builtinFns <- gets builtins
    case lookup name builtinFns of
        Just fn -> liftIO $ fn args
        Nothing -> fail "no built-in with name"

evalCall (Function name params body) args = do
    previousEnv <- get
    previousScopes <- gets scopes
    let scope = Map.union (Map.fromList $ zip params args) (last previousScopes)

    callCC $ \returnCont -> do
        let returnHandler = cleanup previousEnv $ \returnValue -> returnCont returnValue

        modify $ \e -> e{ frames = name : frames e, fnReturn = returnHandler, scopes = scope : previousScopes }
        evalBlock body
        returnHandler None

        return None

evalCall v _ = do
    s <- liftIO $ str v
    fail $ "Unable to call " ++ s

parseEval :: String -> String -> Evaluator ()
parseEval _ code = do
    let statements = parse code
    eval statements

cleanup :: Environment -> (Value -> Evaluator ()) -> Value -> Evaluator ()
cleanup previous action value = do
    put previous
    action value

main :: IO ()
main = do
    [filename] <- getArgs
    code <- readFile filename

    _ <- runStateT (runReaderT (runContT (parseEval filename code) return) defaultFlowControl) defaultEnv
    return ()
