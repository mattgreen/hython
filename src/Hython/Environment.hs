module Hython.Environment
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont hiding (cont)
import Data.HashMap.Strict (HashMap)

import Language.Python.Core

data Frame = Frame String SymbolTable

type SymbolTable = HashMap String Value

type Evaluator = ContT () (ReaderT Config (StateT Environment IO))
type EvaluatorCont = () -> Evaluator ()
type EvaluatorReturnCont = Value -> Evaluator ()
type EvaluatorExceptCont = Value -> Evaluator ()

data Environment = Environment {
    currentException :: Value,
    exceptHandler :: EvaluatorExceptCont,
    mainModule :: ModuleInfo,
    frames :: [Frame],
    scopes :: [SymbolTable],
    builtins :: [(String, Value)],
    fnReturn :: EvaluatorReturnCont,
    loopBreak :: EvaluatorCont,
    loopContinue :: EvaluatorCont
}

data Config = Config {
    tracingEnabled :: Bool
}

