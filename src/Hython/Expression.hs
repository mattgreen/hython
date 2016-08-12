module Hython.Expression
    ( evalExpr
    , evalParam
    )
where

import Control.Monad (forM)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR, xor)
import Data.Fixed (mod')
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Safe (atMay)

import Language.Python

import Hython.Builtins (getAttr)
import Hython.Call (call)
import Hython.Environment (MonadEnv, getClosingEnv, lookupName)
import Hython.Ref
import Hython.Types

evalExpr :: (MonadCont m, MonadEnv Object m, MonadIO m, MonadInterpreter m) => Expression -> m Object
evalExpr (As {}) = unimplemented "as"

evalExpr (Attribute expr attr) = do
    target  <- evalExpr expr
    mobj    <- getAttr attr target

    case mobj of
        Just obj    -> return obj
        Nothing     -> do
            raise "TypeError" ("object has no attribute '" ++ T.unpack attr ++ "'")
            return None

evalExpr (BinOp (ArithOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case (op, lhs, rhs) of
        (Add, Int l, Int r)         -> newInt (l + r)
        (Add, Float l, Float r)     -> newFloat (l + r)
        (Add, String l, String r)   -> newString $ T.append l r
        (Add, l@(Object {}), r)     -> invoke l "__add__" [r]
        (Sub, Int l, Int r)         -> newInt (l - r)
        (Sub, Float l, Float r)     -> newFloat (l - r)
        (Mul, Int l, Int r)         -> newInt (l * r)
        (Mul, Float l, Float r)     -> newFloat (l * r)
        (Mul, Int l, String r)      -> newString $ T.replicate (fromInteger l) r
        (Mul, String _, Int _)      -> evalExpr (BinOp (ArithOp op) rightExpr leftExpr)
        (Mul, Int _, List _)        -> evalExpr (BinOp (ArithOp op) rightExpr leftExpr)
        (Mul, l@(Object {}), r)     -> invoke l "__mul__" [r]
        (Mul, l, r@(Object {}))     -> invoke r "__mul__" [l]
        (Div, Int l, Int r)         -> newFloat (fromInteger l / fromInteger r)
        (Div, Float l, Float r)     -> newFloat (l / r)
        (Mod, Int l, Int r)         -> newInt (l `mod` r)
        (Mod, Float l, Float r)     -> newFloat (l `mod'` r)
        (FDiv, Int l, Int r)        -> newInt (floorInt (fromIntegral l / fromIntegral r))
        (FDiv, Float l, Float r)    -> newFloat (fromInteger (floor (l / r)))
        (Pow, Int l, Int r)
            | r < 0                 -> newFloat (fromIntegral l ^^ r)
            | otherwise             -> newInt (l ^ r)
        (Pow, Float l, Float r)     -> newFloat (l ** r)
        (_, Float _, Int r)         -> evalExpr (BinOp (ArithOp op) leftExpr (constantF $ fromIntegral r))
        (_, Int l, Float _)         -> evalExpr (BinOp (ArithOp op) (constantF $ fromIntegral l) rightExpr)
        _                           -> do
            raise "SystemError" ("unsupported operand type " ++ show op)
            return None
  where
    constantF f = Constant (ConstantFloat f)
    floorInt = floor :: Double -> Integer

evalExpr (BinOp (BitOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case (op, lhs, rhs) of
        (BitAnd, Int l, Int r)      -> newInt (l .&. r)
        (BitOr, Int l, Int r)       -> newInt (l .|. r)
        (BitXor, Int l, Int r)      -> newInt (l `xor` r)
        (LShift, Int l, Int r)      -> newInt (l `shiftL` fromIntegral r)
        (RShift, Int l, Int r)      -> newInt (l `shiftR` fromIntegral r)
        _                           -> do
            raise "SystemError" ("unsupported operand type " ++ show op)
            return None

evalExpr (BinOp (BoolOp op) leftExpr rightExpr) = do
    lhs <- evalExpr leftExpr
    lTruthy <- isTruthy lhs
    case (op, lTruthy) of
        (And, False) -> return lhs
        (And, True)  -> evalExpr rightExpr
        (Or,  False) -> evalExpr rightExpr
        (Or,  True)  -> return lhs

evalExpr (BinOp (CompOp operator) leftExpression rightExpression) = do
    lhs <- evalExpr leftExpression
    go operator lhs rightExpression
  where
    go :: (MonadCont m, MonadEnv Object m, MonadIO m, MonadInterpreter m)
          => ComparisonOperator -> Object -> Expression -> m Object
    -- for chained comparison, e.g. 1 < 2 < 3
    go op lhs (BinOp (CompOp rop) rl rr) = do
        rlhs <- evalExpr rl
        comp <- compareObjs op lhs rlhs
        case comp of
            Bool True -> go rop rlhs rr
            _         -> newBool False
    go op lhs rightExpr = do
        rhs <- evalExpr rightExpr
        compareObjs op lhs rhs

evalExpr (Call expr argExprs) = do
    target      <- evalExpr expr
    args        <- concat <$> mapM evalArg argExprs
    kwargs      <- concat <$> mapM evalKWArg argExprs
    call target args kwargs

  where
    evalArg (Arg e) = do
        obj <- evalExpr e
        return [obj]

    evalArg (StarArg e) = do
        obj <- evalExpr e
        case obj of
            Object {}   -> do
                (List ref) <- invoke obj "__rawitems__" []
                readRef ref
            _           -> do
                raise "TypeError" "argument after * must be a sequence"
                return []

    evalArg _ = return []

    evalKWArg (KeywordArg name e) = do
        obj <- evalExpr e
        return [(name, obj)]

    evalKWArg (DoubleStarArg e) = do
        obj <- evalExpr e
        case obj of
            (Dict ref)  -> do
                dict <- readRef ref
                forM (IntMap.elems dict) $ \(key, value) ->
                    case key of
                        (String s)  -> return (s, value)
                        _           -> do
                            raise "TypeError" "keyword args must be strings"
                            return (T.empty, None)

            _           -> do
                raise "TypeError" "argument after ** must be a mapping"
                return []

    evalKWArg _ = return []

evalExpr (Constant c) = case c of
    ConstantNone        -> newNone
    ConstantBool b      -> newBool b
    ConstantBytes b     -> newBytes b
    ConstantFloat f     -> newFloat f
    ConstantImag i      -> newImag i
    ConstantInt i       -> newInt i
    ConstantString s    -> newString s

evalExpr (DictDef exprs) = do
    items <- forM exprs $ \(keyExpr, valueExpr) -> do
        key     <- evalExpr keyExpr
        value   <- evalExpr valueExpr
        return (key, value)

    newDict items

evalExpr (From {}) = unimplemented "from"

evalExpr (Glob {}) = unimplemented "glob"

evalExpr (LambdaExpr params expr) = do
    env <- getClosingEnv
    p   <- mapM evalParam params
    newLambda p (Return expr) env

evalExpr (ListDef exprs) = do
    items <- mapM evalExpr exprs
    newList items

evalExpr (Name name) = do
    result <- lookupName name
    case result of
        Just obj    -> return obj
        Nothing     -> do
            raise "NameError" ("name '" ++ T.unpack name ++ "' not defined")
            return None

evalExpr (RelativeImport {}) = unimplemented "relative import"

evalExpr (SetDef exprs) = do
    items <- mapM evalExpr exprs
    newSet items

evalExpr (SliceDef {}) = unimplemented "slices"

evalExpr (Subscript expr idxExpr) = do
    target  <- evalExpr expr
    index   <- evalExpr idxExpr
    case (target, index) of
        (String s, Int i) -> case atMay (T.unpack s) (fromIntegral i) of
            Just c      -> newString $ T.pack [c]
            Nothing     -> do
                raise "IndexError" "index out of range"
                return None

        (obj@(Object {}), key) -> invoke obj "__getitem__" [key]

        _ -> do
            raise "TypeError" "object is not subscriptable"
            return None

evalExpr (TernOp condExpr thenExpr elseExpr) = do
    truthy      <- isTruthy =<< evalExpr condExpr
    expr        <- pure $ if truthy then thenExpr else elseExpr
    evalExpr expr

evalExpr (TupleDef exprs) = do
    items <- mapM evalExpr exprs
    newTuple items

evalExpr (UnaryOp op expr) = do
    obj <- evalExpr expr
    case (op, obj) of
        (Not, Bool b)       -> newBool (not b)
        (Pos, Int i)        -> newInt i
        (Neg, Int i)        -> newInt (-i)
        (Pos, Float f)      -> newFloat f
        (Neg, Float f)      -> newFloat (-f)
        (Complement, Int i) -> newInt (complement i)
        _                   -> do
            raise "SystemError" ("Unsupported operand type: " ++ show op)
            return None

evalExpr (Yield {}) = unimplemented "yield"

evalParam :: (MonadEnv Object m, MonadInterpreter m, MonadCont m) => Param -> m FnParam
evalParam (FormalParam param) = return $ NamedParam param
evalParam (DefaultParam param expr) = do
    obj <- evalExpr expr
    return $ DefParam param obj
evalParam (SplatParam param) = return $ SParam param
evalParam (DoubleSplatParam param) = return $ DSParam param


compareObjs :: MonadInterpreter m => ComparisonOperator -> Object -> Object -> m Object
compareObjs op lhs rhs = case (op, lhs, rhs) of
    -- equality
    (Eq,    l, r)             -> newBool =<< equal l r
    (NotEq, l, r)             -> newBool . not =<< equal l r
    -- "is", "is not", "in", "not in"
    (Is,    l, r)             -> newBool $ l `is` r
    (IsNot, l, r)             -> newBool . not $ l `is` r
    (In,    l, r)             -> invoke r "__contains__" [l]
    (NotIn, l, r)             -> do
        Bool b <- invoke r "__contains__" [l]
        newBool (not b)
    -- conversion
    (_, Bool l, _)          -> compareObjs op (Float (boolToFloat l)) rhs
    (_, Int  l, _)          -> compareObjs op (Float (fromIntegral l)) rhs
    (_, _, Bool r)          -> compareObjs op lhs (Float (boolToFloat r))
    (_, _, Int  r)          -> compareObjs op lhs (Float (fromIntegral r))
    -- comparison
    (_, Bytes  l, Bytes  r)   -> applyOp op l r
    (_, Float  l, Float  r)   -> applyOp op l r
    (_, String l, String r)   -> applyOp op l r
    -- error cases
    _ -> do
        raise "TypeError" "unorderable types"
        return None
  where
    boolToFloat True  = 1
    boolToFloat False = 0
    applyOp :: (Ord a, MonadInterpreter m) => ComparisonOperator -> a -> a -> m Object
    applyOp c a b = newBool $ compOp c a b
    compOp LessThan = (<)
    compOp LessThanEq = (<=)
    compOp GreaterThan = (>)
    compOp GreaterThanEq = (>=)
    compOp _ = error "compOp: Eq, NotEq, Is, IsNot, In, NotIn should be handled"


is :: Object -> Object -> Bool
is None None                = True
is (Bool l) (Bool r)        = l == r
is (Class l) (Class r)      = classId l == classId r
is (Object l) (Object r)    = objectId l == objectId r
is (Module l) (Module r)    = moduleId l == moduleId r
is _ _ = False

unimplemented :: (MonadInterpreter m) => String -> m Object
unimplemented expr = do
    raise "SystemError" $ expr ++ " not yet implemented"
    return None
