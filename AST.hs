module AST where

import Data.Complex

data Statement =
    Assignment String Expression
    | Expression Expression
    | If [IfClause] [Statement]
    | Def String [String] [Statement]
    | Return Expression
    | While Expression [Statement]
    | Break
    | Continue
    | Pass
    | Assert Expression
    deriving(Eq, Show)

data IfClause = IfClause Expression [Statement] deriving (Eq, Show)

data Expression =
    Call String [Expression]
    | Variable String
    | BinOp Operator Expression Expression
    | Constant Value
    deriving(Eq, Show)

data Value =
    String String
    | Int Integer
    | Float Double
    | Imaginary (Complex Double)
    | Bool Bool
    | Function String [String] [Statement]
    | None
    deriving(Eq, Show)

data Operator
    = ArithOp ArithmeticOperator
    | BoolOp BooleanOperator
    deriving(Eq, Show)

data ArithmeticOperator = Add | Sub | Mul | Div
    deriving(Eq, Show)

data BooleanOperator = Eq | NotEq | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    deriving(Eq, Show)

