module Hython.Environment
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont hiding (cont)
import Data.HashMap.Strict (HashMap)

import Language.Python.Core

data Frame = Frame String Scope

type Interpreter = ContT () (ReaderT Config (StateT Environment IO))
type InterpreterCont = () -> Interpreter ()
type InterpreterReturnCont = Object -> Interpreter ()
type InterpreterExceptCont = Object -> Interpreter ()

data Environment = Environment {
    currentException :: Object,
    currentFilename :: String,
    exceptHandler :: InterpreterExceptCont,
    frames :: [Frame],
    fnReturn :: InterpreterReturnCont,
    loopBreak :: InterpreterCont,
    loopContinue :: InterpreterCont
}

type SymbolTable = HashMap String Object

data Scope = Scope {
    enclosingScopes :: [SymbolTable],
    globalScope     :: SymbolTable,
    builtinScope    :: SymbolTable
} deriving (Show)

data Config = Config {
    tracingEnabled :: Bool
}

