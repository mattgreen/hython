module AST where

data Statement =
    Assignment String Expression
    | Expression Expression
    | If Expression [Statement] [Statement]
    | Def String [String] [Statement]
    | Return Expression
    | While Expression [Statement]
    | Break
    deriving(Eq, Show)

data Expression =
    Call String [Expression]
    | Variable String
    | BinOp Operator Expression Expression
    | Constant Value
    deriving(Eq, Show)

data Value =
    String String
    | Int Integer
    | Bool Bool
    | Function [String] [Statement]
    | None
    deriving(Eq, Show)

data Operator = Add | Sub | Mul | Div | Eq | NotEq
    deriving(Eq, Show)

