{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language where

import Data.Complex
import Data.IORef
import Data.List
import Data.Map (Map)
import Text.Printf

data Statement
    = Assignment Expression Expression
    | Expression Expression
    | If [IfClause] [Statement]
    | Def String [String] [Statement]
    | Del Expression
    | Return Expression
    | While Expression Statements Statements
    | Break
    | Continue
    | Pass
    | Assert Expression Expression
    | ClassDef String [Expression] Statements
    | ModuleDef Statements
    deriving(Eq, Show)

type Expressions = [Expression]
type Statements = [Statement]
type Values = [Value]

data IfClause = IfClause Expression [Statement] deriving (Eq, Show)

data Expression
    = Call Expression [Expression]
    | Attribute Expression String
    | Name String
    | Constant Value
    | Subscript Expression Expression
    | UnaryOp UnaryOperator Expression
    | BinOp Operator Expression Expression
    | TernOp Expression Expression Expression
    | TupleDef Expressions
    | SliceDef Expression Expression Expression
    | ListDef Expressions
    deriving(Eq, Show)

data Value
    = String String
    | Int Integer
    | Float Double
    | Imaginary (Complex Double)
    | Bool Bool
    | BuiltinFn String
    | Function String [String] [Statement]
    | Class String (IORef AttributeDict)
    | Object Value (IORef AttributeDict)
    | Slice Value Value Value
    | Tuple Values
    | None
    deriving(Eq)

type AttributeDict = Map String Value

data UnaryOperator
    = Not
    | Splat
    | Pos
    | Neg
    | Complement
    deriving(Eq, Show)

data Operator
    = ArithOp ArithmeticOperator
    | BitOp BitOperator
    | BoolOp BooleanOperator
    | CompOp ComparisonOperator
    deriving(Eq, Show)

data ArithmeticOperator
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | FDiv
    | Pow
    deriving(Eq, Show)

data BitOperator
    = BitAnd
    | BitOr
    | BitXor
    | LShift
    | RShift
    deriving(Eq, Show)

data BooleanOperator
    = And
    | Or
    deriving(Eq, Show)

data ComparisonOperator
    = Eq
    | NotEq
    | LessThan
    | LessThanEq
    | GreaterThan
    | GreaterThanEq
    deriving(Eq, Show)

instance Show (IORef a) where
    show _ = "<ioref>"

instance Show Value where
    show None                   = "None"
    show (Bool v)               = show v
    show (String v)             = v
    show (Int v)                = show v
    show (Float v)              = show v
    show (Imaginary v)
        | realPart v == 0       = show (imagPart v) ++ "j"
        | otherwise             = show v
    show (Function name _ _)    = printf "<%s>" name
    show (BuiltinFn name)       = printf "<built-in function %s>" name
    show (Class name _)         = printf "<class '__main__.%s'>" name
    show (Object (Class name _) _) = printf "<%s object>" name
    show (Object _ _)           = "<invalid object>"
    show (Slice start end stride) =
        printf "slice(%s, %s, %s)" (show start) (show end) (show stride)
    show (Tuple values) =
        printf "(%s%s)" (intercalate ", " stringValues) trailer
      where
        stringValues = map show values
        trailer = case values of
            [_]   -> ","
            _     -> ""
class Truthy a where
    isTrue :: a -> Bool

instance Truthy Value where
    isTrue (Int 0)      = False
    isTrue (Bool False) = False
    isTrue (None)       = False
    isTrue _            = True
