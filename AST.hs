module AST where

data Expression =
    Call String Expression
    | String String
    | Int Integer
    | Bool Bool
    | Variable String
    | BinOp Operator Expression Expression
    | None
    deriving(Eq, Show)

data Operator = Add | Sub | Mul | Div | Eq | NotEq
    deriving(Eq, Show)

data Statement =
    Assignment String Expression
    | Block [Statement]
    | Expression Expression
    | If Expression [Statement]
    deriving(Eq, Show)
