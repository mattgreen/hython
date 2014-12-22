{-# LANGUAGE FlexibleContexts #-}

module Hython.Interpreter (interpret, parse)
where

import Prelude hiding (break)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Control.Monad.Trans.Cont hiding (cont)
import Data.Bits
import Data.Fixed
import Data.IORef
import Data.Maybe
import Debug.Trace
import System.Environment
import System.Exit
import Text.Printf

import qualified Hython.AttributeDict as AttributeDict
import Hython.Builtins
import Hython.Classes
import Hython.Environment
import Hython.Frame
import Hython.InterpreterState
import Hython.Module
import Hython.NameResolution
import Language.Python.Core
import Language.Python.Parser

unimplemented :: String -> Interpreter ()
unimplemented s = raiseError "NotImplementedError" (s ++ " not yet implemented")

defaultConfig :: IO Config
defaultConfig = do
    tracing <- lookupEnv "TRACE"

    return Config {
        tracingEnabled = isJust tracing
    }

defaultState :: String -> IO InterpreterState
defaultState path = do
    builtinsList    <- Hython.Builtins.builtins
    mainModuleScope <- AttributeDict.empty
    newLocalScope   <- AttributeDict.empty
    newBuiltinScope <- AttributeDict.fromList builtinsList

    let scope = Scope {
        localScope = newLocalScope,
        moduleScope = mainModuleScope,
        builtinScope = newBuiltinScope,
        activeScope = ModuleScope
    }

    let mainModule = Module "__main__" path mainModuleScope

    return InterpreterState {
        currentException = None,
        exceptHandler = defaultExceptionHandler,
        frames = [Frame "<module>" scope],
        fnReturn = defaultReturnHandler,
        modules = [mainModule],
        currentModule = mainModule,
        loopBreak = defaultBreakHandler,
        loopContinue = defaultContinueHandler
    }

defaultExceptionHandler :: Object -> Interpreter ()
defaultExceptionHandler _exception = liftIO $ do
    putStrLn "Exception: <msg>"
    exitFailure

defaultBreakHandler :: () -> Interpreter ()
defaultBreakHandler () = raiseError "SyntaxError" "'break' outside loop"

defaultContinueHandler :: () -> Interpreter ()
defaultContinueHandler () = raiseError "SyntaxError" "'continue' not properly in loop"

defaultReturnHandler :: Object -> Interpreter ()
defaultReturnHandler _ = raiseError "SyntaxError" "'return' outside function"

raiseError :: String -> String -> Interpreter ()
raiseError errorClassName message = do
    errorClass <- evalExpr (Name errorClassName)
    exception <- evalCall errorClass [String message]
    liftIO $ putStrLn message

    handler <- gets exceptHandler
    handler exception

eval :: Statement -> Interpreter ()
eval (Def name params body) = do
    scope <- currentScope
    liftIO $ bindName name function scope
    return ()

  where
    function = Function name params body

eval (ModuleDef statements) = evalBlock statements

eval (ClassDef name bases statements) = do
    baseClasses <- mapM evalExpr bases
    dict <- liftIO AttributeDict.empty
    evalBlockWithNewScope statements dict

    scope <- currentScope
    liftIO $ bindName name (Class name baseClasses dict) scope
    return ()

eval (Assignment (Name name) expr) = do
    value <- evalExpr expr
    scope <- currentScope
    liftIO $ bindName name value scope
    return ()

eval (Assignment (Attribute var attr) expr) = do
    value <- evalExpr expr
    target <- evalExpr var
    liftIO $ setAttr attr value target

eval (Assignment{}) = raiseError "SyntaxError" "invalid assignment"

eval (Break) = do
    break <- gets loopBreak
    break ()

eval (Continue) = do
    continue <- gets loopContinue
    continue ()

-- Needs EH to implement iterator protocol
eval (For {}) = do
    unimplemented "for keyword"
    return ()

eval (Global {}) = do
    unimplemented "global keyword"
    return ()

eval (If clauses elseBlock) = evalClauses clauses
  where
    evalClauses [] = evalBlock elseBlock
    evalClauses (IfClause condition block : rest) = do
        result <- evalExpr condition
        if isTrue result
            then evalBlock block
            else evalClauses rest

eval (Import exprs) = mapM_ load exprs
  where
    load (Name path) = loadAndBind path Nothing
    load (As (Name path) (Name name)) = loadAndBind path (Just name)

    loadAndBind path binding = do
        newModule <- loadModule path $ \code dict ->
            evalBlockWithNewScope (parse code) dict

        let name = fromMaybe (moduleName newModule) binding

        scope <- currentScope
        liftIO $ bindName name (ModuleObj newModule) scope

eval (ImportFrom _ _) = do
    unimplemented "from...import keyword"
    return ()

eval (Nonlocal {}) = do
    unimplemented "nonlocal keyword"
    return ()

eval (Raise expr _from) = do
    exception <- evalExpr expr
    baseException <- evalExpr (Name "BaseException")

    if isSubClass (classOf exception) baseException
        then do
            modify $ \s -> s { currentException = exception }

            handler <- gets exceptHandler
            handler exception
        else raiseError "TypeError" "must raise subclass of BaseException"

eval (Reraise) = do
    exception <- gets currentException

    case exception of
        None -> raiseError "RuntimeError" "No active exception to reraise"

        _ -> do
            handler <- gets exceptHandler
            handler exception

eval (Return expression) = do
    value <- evalExpr expression

    returnCont <- gets fnReturn
    returnCont value

eval (Try exceptClauses block elseBlock finallyBlock) = do
    state <- get
    previousHandler <- gets exceptHandler

    exception <- callCC $ \handler -> do
        modify $ \s -> s { exceptHandler = handler, fnReturn = chain s fnReturn, loopBreak = chain s loopBreak, loopContinue = chain s loopContinue }
        evalBlock block
        return None

    -- Unwind stack
    modify $ \s -> s { exceptHandler = previousHandler, frames = unwindTo (frames s) (length $ frames state) }

    -- Search for matching handler
    handled <- case exception of
        None -> do
            evalBlock elseBlock
            return True

        _ -> do
            clause <- getClause exceptClauses exception
            case clause of
                Just (ExceptClause _ name handlerBlock) -> do
                    let exceptionBound = name /= ""

                    modify $ \s -> s { exceptHandler = chainExceptHandler previousHandler (exceptHandler s) }

                    when exceptionBound $ do
                        scope <- currentScope
                        liftIO $ bindName name exception scope
                        return ()
                    evalBlock handlerBlock

                    modify $ \s -> s { exceptHandler = previousHandler }

                    return True

                Nothing -> return False

    modify $ \s -> s { exceptHandler = previousHandler }
    evalBlock finallyBlock

    unless handled $
        previousHandler exception

  where
    chain state fn arg = do
        let handler = fn state
        evalBlock finallyBlock
        handler arg

    chainExceptHandler previous handler arg = do
        modify $ \s -> s { exceptHandler = previous }
        evalBlock finallyBlock
        handler arg

    getClause [] _ = return Nothing
    getClause (c@(ExceptClause classExpr _ _):clauses) exception = do
        cls <- evalExpr classExpr
        if isSubClass (classOf exception) cls
            then return $ Just c
            else getClause clauses exception

    unwindTo stackFrames depth
      | length stackFrames > depth  = unwindTo (tail stackFrames) depth
      | otherwise                   = stackFrames

eval (While condition block elseBlock) = do
    state <- get

    callCC $ \breakCont ->
        fix $ \loop -> do
            callCC $ \continueCont -> do
                let breakHandler = restoreHandler state breakCont
                let continueHandler = restoreHandler state continueCont

                modify $ \s -> s { loopBreak = breakHandler, loopContinue = continueHandler }

                result <- evalExpr condition
                unless (isTrue result) $ do
                    evalBlock elseBlock
                    breakHandler ()
                evalBlock block
            loop
  where
    -- TODO: shouldn't we be putting the previous break/continue back?
    restoreHandler state cont value = do
        modify $ \s -> s { exceptHandler = exceptHandler state }
        cont value

eval (With {}) = do
    unimplemented "with keyword"
    return ()

eval (Pass) = return ()

eval (Assert e _) = do
    result <- evalExpr e

    unless (isTrue result) $
        raiseError "AssertionError" ""

eval (Del (Name name)) = do
    scope <- currentScope
    liftIO $ unbindName name scope
    return ()

eval (Expression e) = do
    _ <- evalExpr e
    return ()

evalExpr :: Expression -> Interpreter Object
evalExpr (As expr binding) = do
    value <- evalExpr expr
    scope <- currentScope

    case binding of
        Name n  -> do
            liftIO $ bindName n value scope
            return ()
        _       -> raiseError "SystemError" "unhandled binding type"

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

evalExpr (Call (Attribute expr name) args) = do
    obj <- evalExpr expr
    evalArgs <- mapM evalExpr args
    fn <- liftIO $ getAttr name obj

    case fn of
        Just f -> case obj of
            (ModuleObj m)   -> evalCall f evalArgs
            (Object {})     -> evalCall f (obj : evalArgs)
            _               -> do
                raiseError "SystemError" "trying to call a non-callable object"
                return None

        Nothing -> do
            raiseError "AttributeError" (errorMsgFor obj)
            return None
  where
      errorMsgFor obj = printf "object has no attribute '%s'" name

evalExpr (Call e args) = do
    f <- evalExpr e
    evalArgs <- mapM evalExpr args
    evalCall f evalArgs

evalExpr (Lambda {}) = do
    unimplemented "lambda exprs"
    return None

evalExpr (Attribute target name) = do
    receiver <- evalExpr target
    attribute <- liftIO $ getAttr name receiver
    case attribute of
        Just v  -> return v
        Nothing -> do
            raiseError "AttributeError" (printf "object has no attribute '%s'" name)
            return None

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
    evalSubscript (List {}) _ = do
        raiseError "TypeError" "list indicies must be integers"
        return None
    evalSubscript (Tuple values) (Int i) = return $ values !! fromIntegral i
    evalSubscript (Tuple {}) _ = do
        raiseError "TypeError" "tuple indicies must be integers"
        return None
    evalSubscript (String s) (Int i) = return $ String [s !! fromIntegral i]
    evalSubscript _ _ = do
        raiseError "TypeError" "object is not subscriptable"
        return None

evalExpr (TernOp condExpr thenExpr elseExpr) = do
    condition <- evalExpr condExpr
    evalExpr $ if isTrue condition
        then thenExpr
        else elseExpr

evalExpr (TupleDef exprs) = do
    values <- mapM evalExpr exprs
    return $ Tuple values

evalExpr (From {}) = do
    unimplemented "from expr"
    return None

evalExpr (Yield {}) = do
    unimplemented "yield expr"
    return None

evalExpr (Glob) = do
    unimplemented "glob"
    return None

evalExpr (RelativeImport _ _) = do
    unimplemented "relative import"
    return None

evalExpr (Name name) = do
    scope   <- currentScope
    obj     <- liftIO $ lookupName name scope

    when (isNothing obj) $
        raiseError "NameError" (printf "name '%s' is not defined" name)

    return $ fromJust obj

evalExpr (Constant c) = return c

evalBlock :: [Statement] -> Interpreter ()
evalBlock = mapM_ traceEval
  where
    traceEval :: Statement -> Interpreter ()
    traceEval s = do
        tracing <- asks tracingEnabled
        if tracing
            then (trace $ traceStmt s) eval s
            else eval s

    traceStmt s = "*** Evaluating: " ++ show s

evalCall :: Object -> [Object] -> Interpreter Object
evalCall cls@(Class {}) args = do
    object <- liftIO $ newObject cls

    ctor <- liftIO $ getAttr "__init__" cls
    _ <- case ctor of
        Just f  -> evalCall f (object : args)
        Nothing -> return None

    return object

evalCall (BuiltinFn name) args = do
    let fn = lookup name builtinFunctions
    when (isNothing fn) $
        raiseError "NameError" ("no built-in with name " ++ name)

    liftIO $ fromJust fn args

evalCall (Function name params body) args = do
    state <- get

    when (length params /= length args) $
        raiseError "TypeError" arityErrorMsg

    symbols <- liftIO $ AttributeDict.fromList $ zip (map unwrapArg params) args

    scope <- currentScope
    let functionScope = scope { localScope = symbols, activeScope = LocalScope }

    callCC $ \returnCont -> do
        let returnHandler returnValue = do
            put state
            returnCont returnValue

        pushFrame name scope { localScope = symbols, activeScope = LocalScope }
        modify $ \s -> s { fnReturn = returnHandler }
        evalBlock body
        returnHandler None

        return None

  where
    argCount = length args
    paramCount = length params
    arityErrorMsg = printf "%s() takes exactly %d arguments (%d given)" name paramCount argCount

    unwrapArg (PositionalArg n) = n

evalCall v _ = do
    s <- liftIO $ str v
    raiseError "SystemError" ("don't know how to call " ++ s)
    return None

evalBlockWithNewScope :: [Statement] -> AttributeDict -> Interpreter ()
evalBlockWithNewScope statements dict = do
    scope <- currentScope
    updateScope $ scope { localScope = dict, activeScope = LocalScope }
    evalBlock statements
    updateScope scope

interpret :: String -> String -> IO ()
interpret path code = do
    config  <- defaultConfig
    state   <- defaultState path

    _ <- runStateT (runReaderT (runContT parseEval return) config) state
    return ()

  where
    parseEval = evalBlock (parse code)
