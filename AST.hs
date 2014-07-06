module AST where

data Expression =
    Call String Expression
    | BinOp Operator Expression Expression
    | String String
    | Int Integer
    | None
    deriving(Show)

data Operator = Add | Sub | Mul | Div
    deriving(Show)
