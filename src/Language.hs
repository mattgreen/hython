{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language where

import Data.Complex
import Data.IORef
import Data.Map (Map)

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
    | Assert Expression
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
    | UnaryOp UnaryOperator Expression
    | BinOp Operator Expression Expression
    | Constant Value
    | TupleDef Expressions
    | ListDef Expressions
    deriving(Eq, Show)

data Value
    = String String
    | Int Integer
    | Float Double
    | Imaginary (Complex Double)
    | Bool Bool
    | Function String [String] [Statement]
    | Class String (IORef AttributeDict)
    | Object Value (IORef AttributeDict)
    | Tuple Values
    | None
    deriving(Eq, Show)

type AttributeDict = Map String Value

instance Show (IORef a) where
    show _ = "<ioref>"

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

data ComparisonOperator = Eq | NotEq | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    deriving(Eq, Show)

