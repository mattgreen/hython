module AST where

data Expression =
    Call String Expression
    | Block [Expression]
    | Assignment String Expression
    | LocalVar String
    | BinOp Operator Expression Expression
    | String String
    | Int Integer
    | True
    | False
    | None
    deriving(Show)

data Operator = Add | Sub | Mul | Div | Eq | NotEq
    deriving(Show)
