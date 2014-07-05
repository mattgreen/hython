module AST where

data Expression =
    Seq [Expression]
    | Call String Expression
    | Add Expression Expression
    | String String
    | Int Integer
    | None
    deriving(Show)
