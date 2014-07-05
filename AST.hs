module AST where

data Expression =
    Call String Expression
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression
    | String String
    | Int Integer
    | None
    deriving(Show)
