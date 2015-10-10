module Hython.Expression (evalExpr) where

import Control.Monad (forM, zipWithM)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR, xor)
import Data.Fixed (mod')
import qualified Data.IntMap as IntMap
import Data.IORef (readIORef)
import Data.Text (pack)
import qualified Data.Text as T
import Safe (atMay)

import Language.Python

import Hython.Builtins (getAttr)
import Hython.Call (call)
import Hython.Environment (lookupName)
import Hython.Types

evalExpr :: (MonadCont m, MonadEnv m, MonadIO m, MonadInterpreter m) => Expression -> m Object
evalExpr (As {}) = unimplemented "as"

evalExpr (Attribute expr attr) = do
    target  <- evalExpr expr
    mobj    <- getAttr attr target

    case mobj of
        Just obj    -> return obj
        Nothing     -> do
            raise "TypeError" ("object has no attribute '" ++ attr ++ "'")
            return None

evalExpr (BinOp (ArithOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case (op, lhs, rhs) of
        (Add, Int l, Int r)         -> newInt (l + r)
        (Add, Float l, Float r)     -> newFloat (l + r)
        (Add, String l, String r)   -> newString $ T.append l r
        (Add, List l, List r)       -> do
            left    <- liftIO $ readIORef l
            right   <- liftIO $ readIORef r
            newList (left ++ right)
        (Add, Tuple l, Tuple r)     -> newTuple (l ++ r)
        (Sub, Int l, Int r)         -> newInt (l - r)
        (Sub, Float l, Float r)     -> newFloat (l - r)
        (Mul, Int l, Int r)         -> newInt (l * r)
        (Mul, Float l, Float r)     -> newFloat (l * r)
        (Mul, Int l, String r)      -> newString $ T.replicate (fromInteger l) r
        (Mul, String _, Int _)      -> evalExpr (BinOp (ArithOp op) rightExpr leftExpr)
        (Mul, List l, Int r)        -> do
            items <- liftIO $ readIORef l
            newList $ concat $ replicate (fromInteger r) items
        (Mul, Int _, List _)        -> evalExpr (BinOp (ArithOp op) rightExpr leftExpr)
        (Mul, Tuple l, Int r)       -> newTuple $ concat $ replicate (fromInteger r) l
        (Mul, Int _, Tuple _)       -> evalExpr (BinOp (ArithOp op) rightExpr leftExpr)
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
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case (op, lhs, rhs) of
        (And, Bool l, Bool r)   -> newBool (l && r)
        (And, l, r)             -> do
            left    <- isTruthy l
            right   <- isTruthy r
            return $ if left && right
                         then r
                         else l
        (Or, Bool l, Bool r)    -> newBool (l || r)
        (Or, l, r)              -> do
            left    <- isTruthy l
            return $ if left
                         then l
                         else r

evalExpr (BinOp (CompOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case (op, lhs, rhs) of
        (Eq, l, r) -> newBool =<< equal l r
        (NotEq, l, r) -> do
            b <- equal l r
            newBool $ not b
        (LessThan, Float l, Float r)        -> newBool (l < r)
        (LessThan, Int l, Int r)            -> newBool (l < r)
        (LessThanEq, Float l, Float r)      -> newBool (l <= r)
        (LessThanEq, Int l, Int r)          -> newBool (l <= r)
        (GreaterThan, Float l, Float r)     -> newBool (l > r)
        (GreaterThan, Int l, Int r)         -> newBool (l > r)
        (GreaterThanEq, Int l, Int r)       -> newBool (l >= r)
        (GreaterThanEq, Float l, Float r)   -> newBool (l >= r)
        (_, Float _, Int r)                 -> evalExpr (BinOp (CompOp op) leftExpr (constantF r))
        (_, Int l, Float _)                 -> evalExpr (BinOp (CompOp op) (constantF l) rightExpr)
        (In, l, Dict ref)                   -> do
            h <- hash l
            items <- liftIO $ readIORef ref
            newBool $ IntMap.member h items
        (In, l, List ref)                   -> do
            items <- liftIO $ readIORef ref
            results <- mapM (equal l) items
            newBool $ True `elem` results
        (In, l, Set ref)                    -> do
            key <- hash l
            items <- liftIO $ readIORef ref
            newBool $ key `IntMap.member` items
        (In, l, Tuple items)                -> do
            results <- mapM (equal l) items
            newBool $ True `elem` results
        (NotIn, _, _)                       -> do
            (Bool b) <- evalExpr (BinOp (CompOp In) leftExpr rightExpr)
            newBool (not b)
        _ -> do
            raise "SystemError" ("unsupported operand type " ++ show op)
            return None
  where
    constantF i = Constant $ ConstantFloat $ fromIntegral i

    equal (Bool l) (Bool r)             = return $ l == r
    equal (Bytes l) (Bytes r)           = return $ l == r
    equal (Float l) (Float r)           = return $ l == r
    equal (Imaginary l) (Imaginary r)   = return $ l == r
    equal (Int l) (Int r)               = return $ l == r
    equal (String l) (String r)         = return $ l == r
    equal (BuiltinFn l) (BuiltinFn r)   = return $ l == r
    equal (Tuple l) (Tuple r)           =
        if length l /= length r
            then return False
            else do
                results <- zipWithM equal l r
                return $ all (== True) results
    equal (List l) (List r) = do
        left    <- liftIO $ readIORef l
        right   <- liftIO $ readIORef r
        equal (Tuple left) (Tuple right)
    equal (Set l) (Set r) = do
        left    <- liftIO $ readIORef l
        right   <- liftIO $ readIORef r
        equal (Tuple $ IntMap.elems left) (Tuple $ IntMap.elems right)
    equal (Dict l) (Dict r) = do
        left    <- liftIO $ readIORef l
        right   <- liftIO $ readIORef r
        if IntMap.size left /= IntMap.size right
            then return False
            else do
                results <- zipWithM pairEqual (IntMap.elems left) (IntMap.elems right)
                return $ all (== True) results
    equal _ _                           = return False

    pairEqual (lk, lv) (rk, rv) = do
        k <- equal lk rk
        v <- equal lv rv
        return $ k && v

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
            (Tuple objs)    -> return objs
            (List ref)      -> liftIO $ readIORef ref
            _               -> do
                raise "TypeError" "argument after * must be a sequence"
                return []

    evalArg _ = return []

    evalKWArg (KeywordArg name e) = do
        obj <- evalExpr e
        return [(T.pack name, obj)]

    evalKWArg (DoubleStarArg e) = do
        obj <- evalExpr e
        case obj of
            (Dict ref)  -> do
                dict <- liftIO $ readIORef ref
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
    objs <- forM exprs $ \(keyExpr, valueExpr) -> do
        key <- evalExpr keyExpr
        value <- evalExpr valueExpr
        return (key, value)
    newDict objs

evalExpr (From {}) = unimplemented "from"

evalExpr (Glob {}) = unimplemented "glob"

evalExpr (LambdaExpr {}) = unimplemented "lambda"

evalExpr (ListDef exprs) = do
    objs <- mapM evalExpr exprs
    newList objs

evalExpr (Name name) = do
    result <- lookupName (pack name)
    case result of
        Just obj    -> return obj
        Nothing     -> do
            raise "NameError" ("name '" ++ name ++ "' not defined")
            return None

evalExpr (RelativeImport {}) = unimplemented "relative import"

evalExpr (SetDef exprs) = do
    objs <- mapM evalExpr exprs
    newSet objs

evalExpr (SliceDef {}) = unimplemented "slices"

evalExpr (Subscript expr idxExpr) = do
    target  <- evalExpr expr
    index   <- evalExpr idxExpr
    case (target, index) of
        (Dict ref, key) -> do
            items <- liftIO $ readIORef ref
            h <- hash key

            case IntMap.lookup h items of
                Just (_, value) -> return value
                Nothing -> do
                    s <- toStr key
                    raise "KeyError" s
                    return None

        (List ref, Int i) -> do
            items <- liftIO $ readIORef ref
            case atMay items (fromIntegral i) of
                Just obj    -> return obj
                Nothing     -> do
                    raise "TypeError" "index out of range"
                    return None

        (String s, Int i) -> case atMay (T.unpack s) (fromIntegral i) of
            Just c      -> newString $ T.pack [c]
            Nothing     -> do
                raise "IndexError" "index out of range"
                return None

        (Tuple objs, Int i) -> case atMay objs (fromIntegral i) of
            Just obj    -> return obj
            Nothing     -> do
                raise "IndexErrror" "index out of range"
                return None

        _ -> do
            raise "TypeError" "object is not subscriptable"
            return None

evalExpr (TernOp condExpr thenExpr elseExpr) = do
    truthy      <- isTruthy =<< evalExpr condExpr
    expr        <- pure $ if truthy then thenExpr else elseExpr
    evalExpr expr

evalExpr (TupleDef exprs) = do
    objs <- mapM evalExpr exprs
    return $ Tuple objs

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

unimplemented :: (MonadInterpreter m) => String -> m Object
unimplemented expr = do
    raise "SystemError" $ expr ++ " not yet implemented"
    return None
