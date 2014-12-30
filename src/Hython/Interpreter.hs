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
import Hython.Object

import Language.Python
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
    mainModuleEnv   <- AttributeDict.empty
    newLocalEnv     <- AttributeDict.empty
    newBuiltinEnv   <- AttributeDict.fromList builtinsList

    let env = Env {
        localEnv = newLocalEnv,
        moduleEnv = mainModuleEnv,
        builtinEnv = newBuiltinEnv,
        activeEnv = ModuleEnv
    }

    let mainModule = Module "__main__" path mainModuleEnv

    return InterpreterState {
        currentException = None,
        exceptHandler = defaultExceptionHandler,
        frames = [Frame "<module>" env],
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
    env <- currentEnv
    liftIO $ bindName name (Function name params body env) env

eval (ModuleDef statements) = evalBlock statements

eval (ClassDef name bases statements) = do
    baseClasses <- mapM evalExpr bases
    dict <- liftIO AttributeDict.empty
    evalBlockWithNewEnv statements dict

    env <- currentEnv
    liftIO $ bindName name (Class name baseClasses dict) env

eval (Assignment (Name name) expr) = do
    value <- evalExpr expr
    env <- currentEnv
    liftIO $ bindName name value env

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
eval (For {}) = unimplemented "for keyword"

eval (Global {}) = unimplemented "global keyword"

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
    load _ = raiseError "SystemError" "invalid import statement"

    loadAndBind path binding = do
        moduleOrErr <- loadModule path $ \code dict ->
            evalBlockWithNewEnv (parse code) dict

        case moduleOrErr of
            Right newModule -> do
                let name = fromMaybe (moduleName newModule) binding

                env <- currentEnv
                liftIO $ bindName name (ModuleObj newModule) env
            Left err -> raiseError "ImportError" err

eval (ImportFrom (RelativeImport _level (Name path)) [Glob]) = do
    moduleOrErr <- loadModule path $ \code dict ->
        evalBlockWithNewEnv (parse code) dict

    case moduleOrErr of
        Right newModule -> do
            env <- currentEnv
            liftIO $ bindNames (moduleDict newModule) env
        Left err -> raiseError "ImportError" err

eval (ImportFrom {}) = raiseError "SystemError" "invalid import from statement"

eval (Nonlocal {}) = unimplemented "nonlocal keyword"

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
                        env <- currentEnv
                        liftIO $ bindName name exception env
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

eval (With {}) = unimplemented "with keyword"

eval (Pass) = return ()

eval (Assert e _) = do
    result <- evalExpr e

    unless (isTrue result) $
        raiseError "AssertionError" ""

eval (Del (Name name)) = do
    env <- currentEnv
    liftIO $ unbindName name env

eval (Del {}) = raiseError "SystemError" "invalid del statement"

eval (Expression e) = do
    _ <- evalExpr e
    return ()

evalExpr :: Expression -> Interpreter Object
evalExpr (As expr binding) = do
    value <- evalExpr expr
    env <- currentEnv

    case binding of
        Name n  -> liftIO $ bindName n value env
        _       -> raiseError "SystemError" "unhandled binding type"

    return value

evalExpr (UnaryOp op rightExpr) = do
    rhs <- evalExpr rightExpr
    case eval' op rhs of
        Just v  -> return v
        Nothing -> unhandledUnaryOp op rhs
  where
    eval' Not (Bool v)          = Just $ Bool (not v)
    eval' Pos (Int v)           = Just $ Int v
    eval' Pos (Float v)         = Just $ Float v
    eval' Neg (Int v)           = Just $ Int (- v)
    eval' Neg (Float v)         = Just $ Float (- v)
    eval' Complement (Int v)    = Just $ Int (complement v)
    eval' _ _                   = Nothing

evalExpr (BinOp (ArithOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case eval' op lhs rhs of
        Just v  -> return v
        Nothing -> unhandledBinOpExpr op lhs rhs
  where
    eval' Add (Int l) (Int r)           = Just $ Int (l + r)
    eval' Add (Float l) (Float r)       = Just $ Float (l + r)
    eval' Add (String l) (String r)     = Just $ String (l ++ r)
    eval' Sub (Int l) (Int r)           = Just $ Int (l - r)
    eval' Sub (Float l) (Float r)       = Just $ Float (l - r)
    eval' Mul (Int l) (Int r)           = Just $ Int (l * r)
    eval' Mul (Float l) (Float r)       = Just $ Float (l * r)
    eval' Mul (Int l) (String r)        = Just $ String (concat $ replicate (fromInteger l) r)
    eval' Mul l@(String {}) r@(Int {})  = eval' Mul r l
    eval' Div (Int l) (Int r)           = Just $ Float (fromInteger l / fromInteger r)
    eval' Div (Float l) (Float r)       = Just $ Float (l / r)
    eval' Mod (Int l) (Int r)           = Just $ Int (l `mod` r)
    eval' Mod (Float l) (Float r)       = Just $ Float (l `mod'` r)
    eval' FDiv (Int l) (Int r)          = Just $ Int (floorInt (fromIntegral l / fromIntegral r))
    eval' FDiv (Float l) (Float r)      = Just $ Float (fromInteger (floor (l / r)))
    eval' Pow l@(Int {}) r@(Int {})     = Just $ pow [l, r]
    eval' Pow l@(Float {}) r@(Float {}) = Just $ pow [l, r]
    eval' _ l@(Float {}) (Int r)        = eval' op l (Float (fromIntegral r))
    eval' _ (Int l) r@(Float {})        = eval' op (Float (fromIntegral l)) r
    eval' _ _ _                         = Nothing

    floorInt = floor :: Double -> Integer

evalExpr (BinOp (BitOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case eval' op lhs rhs of
        Just v  -> return $ Int v
        Nothing -> unhandledBinOpExpr op lhs rhs
  where
    eval' BitAnd (Int l) (Int r)    = Just $ l .&. r
    eval' BitOr  (Int l) (Int r)    = Just $ l .|. r
    eval' BitXor (Int l) (Int r)    = Just $ xor l r
    eval' LShift (Int l) (Int r)    = Just $ shiftL l (fromIntegral r)
    eval' RShift (Int l) (Int r)    = Just $ shiftR l (fromIntegral r)
    eval' _ _ _                     = Nothing

evalExpr (BinOp (BoolOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case eval' op lhs rhs of
        Just v  -> return v
        Nothing -> unhandledBinOpExpr op lhs rhs
  where
    eval' And (Bool l) (Bool r)     = Just $ Bool (l && r)
    eval' Or (Bool l) (Bool r)      = Just $ Bool (l || r)
    eval' And l r                   = Just $ if isTrue l && isTrue r
                                        then r
                                        else l
    eval' Or l r                    = Just $ if isTrue l
                                        then l
                                        else r

evalExpr (BinOp (CompOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case eval' op lhs rhs of
        Just v  -> return $ Bool v
        Nothing -> unhandledBinOpExpr op lhs rhs
  where
    eval' _ l@(Float {}) (Int r)            = eval' op l (Float (fromIntegral r))
    eval' _ (Int l) r@(Float {})            = eval' op (Float (fromIntegral l)) r
    eval' Eq (Int l) (Int r)                = Just $ l == r
    eval' Eq (Float l) (Float r)            = Just $ l == r
    eval' Eq (String l) (String r)          = Just $ l == r
    eval' Eq (Bool l) (Bool r)              = Just $ l == r
    eval' Eq (None) (None)                  = Just True
    eval' Eq (None) _                       = Just False
    eval' Eq _ (None)                       = Just False
    eval' Eq _ _                            = Just False
    eval' NotEq (Int l) (Int r)             = Just $ l /= r
    eval' NotEq (Float l) (Float r)         = Just $ l /= r
    eval' NotEq (String l) (String r)       = Just $ l /= r
    eval' NotEq (Bool l) (Bool r)           = Just $ l /= r
    eval' NotEq (None) (None)               = Just False
    eval' NotEq (None) _                    = Just True
    eval' NotEq _ (None)                    = Just True
    eval' NotEq _ _                         = Just True
    eval' LessThan (Int l) (Int r)          = Just $ l < r
    eval' LessThan (Float l) (Float r)      = Just $ l < r
    eval' LessThanEq (Int l) (Int r)        = Just $ l <= r
    eval' LessThanEq (Float l) (Float r)    = Just $ l <= r
    eval' GreaterThan (Int l) (Int r)       = Just $ l > r
    eval' GreaterThan (Float l) (Float r)   = Just $ l > r
    eval' GreaterThanEq (Int l) (Int r)     = Just $ l >= r
    eval' GreaterThanEq (Float l) (Float r) = Just $ l >= r
    eval' _ _ _                             = Nothing

evalExpr (Call (Attribute expr name) args) = do
    obj <- evalExpr expr
    evalArgs <- mapM evalExpr args
    fn <- liftIO $ getAttr name obj

    case fn of
        Just f -> case obj of
            (ModuleObj _)   -> evalCall f evalArgs
            (Object {})     -> evalCall f (obj : evalArgs)
            _               -> do
                raiseError "SystemError" "trying to call a non-callable object"
                return None

        Nothing -> do
            raiseError "AttributeError" (errorMsgFor obj)
            return None
  where
      errorMsgFor _ = printf "object has no attribute '%s'" name

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

evalExpr (SetDef {}) = do
    unimplemented "setdef expr"
    return None

evalExpr (DictDef {}) = do
    unimplemented "dictdef expr"
    return None

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
    env     <- currentEnv
    obj     <- liftIO $ lookupName name env

    when (isNothing obj) $
        raiseError "NameError" (printf "name '%s' is not defined" name)

    return $ fromJust obj

evalExpr (Constant c) = return $ asObject c
  where
    asObject (ConstantInt v)     = Int v
    asObject (ConstantFloat v)   = Float v
    asObject (ConstantImag v)    = Imaginary v
    asObject (ConstantString v)  = String v
    asObject (ConstantBool v)    = Bool v
    asObject (ConstantNone)      = None

unhandledUnaryOp :: Show a => a -> Object -> Interpreter Object
unhandledUnaryOp op r = do
    rhs <- liftIO $ str r
    raiseError "SystemError" $ printf msg (show op) rhs
    return None
  where
    msg = "Unsupported operand type for %s: %s"

unhandledBinOpExpr :: Show a => a -> Object -> Object -> Interpreter Object
unhandledBinOpExpr op l r = do
    lhs <- liftIO $ str l
    rhs <- liftIO $ str r
    raiseError "SystemError" $ printf msg (show op) lhs rhs
    return None
  where
    msg = "Unsupported operand type(s) for %s: %s %s"

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

evalCall (Function name params body env) args = do
    state <- get

    when (length params /= length args) $
        raiseError "TypeError" arityErrorMsg

    symbols <- liftIO $ AttributeDict.fromList $ zip (map unwrapArg params) args

    callCC $ \returnCont -> do
        let returnHandler returnValue = do
            put state
            returnCont returnValue

        pushFrame name env { localEnv = symbols, activeEnv = LocalEnv }
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

evalBlockWithNewEnv :: [Statement] -> AttributeDict -> Interpreter ()
evalBlockWithNewEnv statements dict = do
    env <- currentEnv
    updateEnv $ env { localEnv = dict, activeEnv = LocalEnv }
    evalBlock statements
    updateEnv env

interpret :: String -> String -> IO ()
interpret path code = do
    config  <- defaultConfig
    state   <- defaultState path

    _ <- runStateT (runReaderT (runContT parseEval return) config) state
    return ()

  where
    parseEval = evalBlock (parse code)
