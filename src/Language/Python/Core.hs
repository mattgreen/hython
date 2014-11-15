{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Python.Core where

import Data.Complex
import Data.IORef
import Data.HashMap.Strict (HashMap)

data Statement
    = Assignment Expression Expression
    | Expression Expression
    | If [IfClause] [Statement]
    | Def String [String] [Statement]
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
    | Global Expressions
    | Nonlocal Expressions
    | ClassDef String [Expression] Statements
    | ModuleDef Statements
    deriving(Eq, Show)

data IfClause = IfClause Expression [Statement] deriving (Eq, Show)

data ExceptClause
    = ExceptClause Expression String Statements
    deriving (Eq, Show)

type Statements     = [Statement]
type IfClauses      = [IfClause]
type ExceptClauses  = [ExceptClause]

type Expressions = [Expression]
type Values = [Value]

data Expression
    = Call Expression [Expression]
    | Attribute Expression String
    | Name String
    | Constant Value
    | Subscript Expression Expression
    | As Expression Expression
    | Yield Expression
    | From Expression
    | Lambda [String] Expression
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
    | Module ModuleInfo AttributeDict
    | Class String Values AttributeDict
    | Object Value AttributeDict
    | Slice Value Value Value
    | Tuple Values
    | List (IORef Values)
    | None
    deriving(Eq, Show)

type AttributeDict = IORef (HashMap String (IORef Value))

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

data ModuleInfo = ModuleInfo {
    moduleName :: String
} deriving(Eq, Show)

instance Show (IORef a) where
    show _ = "<ioref>"

class Truthy a where
    isTrue :: a -> Bool

instance Truthy Value where
    isTrue (Int 0)      = False
    isTrue (Bool False) = False
    isTrue (None)       = False
    isTrue _            = True
