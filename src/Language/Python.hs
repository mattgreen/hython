module Language.Python where

import Data.Complex

data Statement
    = Assignment Expression Expression
    | Expression Expression
    | If [IfClause] [Statement]
    | FuncDef String [Param] [Statement]
    | Del Expression
    | For Expression Expression Statements Statements
    | Raise Expression Expression
    | Reraise
    | Return Expression
    | Try ExceptClauses Statements Statements Statements
    | While Expression Statements Statements
    | With Expressions Statements
    | Break
    | Continue
    | Pass
    | Assert Expression Expression
    | Global [String]
    | Nonlocal Expressions
    | ClassDef String [Expression] Statements
    | Import [Expression]
    | ImportFrom Expression [Expression]
    deriving(Eq, Show)

data Param
    = FormalParam String
    | DefaultParam String Expression
    | SplatParam String
    | DoubleSplatParam String
    deriving (Eq, Show)

data IfClause = IfClause Expression [Statement] deriving (Eq, Show)

data ExceptClause
    = ExceptClause Expression String Statements
    deriving (Eq, Show)

type Statements     = [Statement]
type IfClauses      = [IfClause]
type ExceptClauses  = [ExceptClause]

type Expressions = [Expression]

data Expression
    = Call Expression [Expression]
    | Attribute Expression String
    | Name String
    | Constant Constant
    | Subscript Expression Expression
    | As Expression Expression
    | Yield Expression
    | Star Expression
    | DoubleStar Expression
    | From Expression
    | Glob
    | RelativeImport Int Expression
    | LambdaExpr [Param] Expression
    | UnaryOp UnaryOperator Expression
    | BinOp Operator Expression Expression
    | TernOp Expression Expression Expression
    | TupleDef Expressions
    | SliceDef Expression Expression Expression
    | ListDef Expressions
    | SetDef Expressions
    | DictDef [(Expression, Expression)]
    deriving(Eq, Show)

data Constant
    = ConstantInt Integer
    | ConstantString String
    | ConstantFloat Double
    | ConstantImag (Complex Double)
    | ConstantBool Bool
    | ConstantNone
    deriving(Eq, Show)

data UnaryOperator
    = Not
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
    | In
    | NotIn
    | Is
    | IsNot
    deriving(Eq, Show)
