module Hython.Environment
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont hiding (cont)
import Data.HashMap.Strict (HashMap)

import Language.Python.Core

data Frame = Frame String Scope

type Evaluator = ContT () (ReaderT Config (StateT Environment IO))
type EvaluatorCont = () -> Evaluator ()
type EvaluatorReturnCont = Value -> Evaluator ()
type EvaluatorExceptCont = Value -> Evaluator ()

data Environment = Environment {
    currentException :: Value,
    exceptHandler :: EvaluatorExceptCont,
    mainModule :: ModuleInfo,
    frames :: [Frame],
    fnReturn :: EvaluatorReturnCont,
    loopBreak :: EvaluatorCont,
    loopContinue :: EvaluatorCont
}

type SymbolTable = HashMap String Value

data Scope = Scope {
    enclosingScopes :: [SymbolTable],
    globalScope     :: SymbolTable,
    builtinScope    :: SymbolTable
} deriving (Show)

data Config = Config {
    tracingEnabled :: Bool
}

