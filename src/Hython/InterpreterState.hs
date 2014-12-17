module Hython.InterpreterState
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont hiding (cont)
import Data.HashMap.Strict (HashMap)

import Language.Python.Core

type Interpreter = ContT () (ReaderT Config (StateT InterpreterState IO))
type InterpreterCont = () -> Interpreter ()
type InterpreterReturnCont = Object -> Interpreter ()
type InterpreterExceptCont = Object -> Interpreter ()

data InterpreterState = InterpreterState
    { currentException :: Object
    , currentFilename :: String
    , exceptHandler :: InterpreterExceptCont
    , frames :: [Frame]
    , fnReturn :: InterpreterReturnCont
    , loopBreak :: InterpreterCont
    , loopContinue :: InterpreterCont
    }

data Frame = Frame String Scope

type SymbolTable = HashMap String Object

data Scope = Scope {
    enclosingScopes :: [SymbolTable],
    globalScope     :: SymbolTable,
    builtinScope    :: SymbolTable
} deriving (Show)

data Config = Config {
    tracingEnabled :: Bool
}

