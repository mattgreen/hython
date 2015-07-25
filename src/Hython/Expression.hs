module Hython.Expression (evalExpr) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR, xor)
import Data.Fixed (mod')
import Data.IORef (readIORef)
import Data.Text (pack)

import Language.Python

import Hython.Builtins (callBuiltin)
import Hython.Object

evalExpr :: (MonadIO m, MonadInterpreter m) => Expression -> m Object
evalExpr (BinOp (ArithOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case (op, lhs, rhs) of
        (Add, Int l, Int r)         -> newInt (l + r)
        (Add, Float l, Float r)     -> newFloat (l + r)
        (Add, String l, String r)   -> newString (l ++ r)
        (Add, List l, List r)       -> do
            left    <- liftIO $ readIORef l
            right   <- liftIO $ readIORef r
            newList (left ++ right)
        (Sub, Int l, Int r)         -> newInt (l - r)
        (Sub, Float l, Float r)     -> newFloat (l - r)
        (Mul, Int l, Int r)         -> newInt (l * r)
        (Mul, Float l, Float r)     -> newFloat (l * r)
        (Mul, Int l, String r)      -> newString (concat $ replicate (fromInteger l) r)
        (Mul, String _, Int _)      -> evalExpr (BinOp (ArithOp op) rightExpr leftExpr)
        (Mul, List l, Int r)        -> do
            items <- liftIO $ readIORef l
            newList $ concat $ replicate (fromInteger r) items
        (Mul, Int _, List _)        -> evalExpr (BinOp (ArithOp op) rightExpr leftExpr)
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
        (And, l, r)             -> return $ if isTruthy l && isTruthy r
                                       then r
                                       else l
        (Or, Bool l, Bool r)    -> newBool (l || r)
        (Or, l, r)              -> return $ if isTruthy l
                                       then l
                                       else r

evalExpr (BinOp (CompOp op) leftExpr rightExpr) = do
    [lhs, rhs] <- mapM evalExpr [leftExpr, rightExpr]
    case (op, lhs, rhs) of
        (Eq, l, r) -> do
            b <- equal l r
            newBool b
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
    equal (List l) (List r) = do
        left    <- liftIO $ readIORef l
        right   <- liftIO $ readIORef r
        if length left /= length right
            then return False
            else do
                results <- zipWithM equal left right
                return $ all (== True) results
    equal _ _                           = return False


evalExpr (Constant c) = case c of
    ConstantNone        -> newNone
    ConstantBool b      -> newBool b
    ConstantBytes b     -> newBytes b
    ConstantFloat f     -> newFloat f
    ConstantImag i      -> newImag i
    ConstantInt i       -> newInt i
    ConstantString s    -> newString s

evalExpr (Call expr argExprs) = do
    callable <- evalExpr expr
    args <- mapM evalExpr argExprs

    case callable of
        (BuiltinFn name)    -> callBuiltin name args
        _                   -> do
            raise "TypeError" "object is not callable"
            return None

evalExpr (ListDef exprs) = do
    objs <- mapM evalExpr exprs
    newList objs

evalExpr (Name name) = do
    result <- lookupName (pack name)
    case result of
        Just obj    -> return obj
        Nothing     -> do
            raise "NameError" "name not defined"
            return None

evalExpr (Subscript expr idxExpr) = do
    target  <- evalExpr expr
    index   <- evalExpr idxExpr
    case (target, index) of
        (List ref, Int i) -> do
            l <- liftIO $ readIORef ref
            return $ l !! fromIntegral i
        (String s, Int i) -> newString [s !! fromIntegral i]
        _ -> do
            raise "TypeError" "object is not subscriptable"
            return None

evalExpr (TernOp condExpr thenExpr elseExpr) = do
    condition <- evalExpr condExpr
    evalExpr $ if isTruthy condition
                   then thenExpr
                   else elseExpr

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


