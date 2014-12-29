module Hython.InterpreterState
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont hiding (cont)

import Hython.Object

type Interpreter = ContT () (ReaderT Config (StateT InterpreterState IO))
type InterpreterCont = () -> Interpreter ()
type InterpreterReturnCont = Object -> Interpreter ()
type InterpreterExceptCont = Object -> Interpreter ()

data InterpreterState = InterpreterState
    { currentException  :: Object
    , exceptHandler     :: InterpreterExceptCont
    , frames            :: [Frame]
    , modules           :: [Module]
    , currentModule     :: Module
    , fnReturn          :: InterpreterReturnCont
    , loopBreak         :: InterpreterCont
    , loopContinue      :: InterpreterCont
    }

data Frame = Frame String Env

data Config = Config {
    tracingEnabled :: Bool
}

