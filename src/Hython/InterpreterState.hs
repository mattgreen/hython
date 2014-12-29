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

data Env = Env
    { localEnv          :: AttributeDict
    , moduleEnv         :: AttributeDict
    , builtinEnv        :: AttributeDict
    , activeEnv         :: ActiveEnv
    } deriving (Show)

data ActiveEnv
    = ModuleEnv
    | LocalEnv
    deriving (Eq, Show)

data Frame = Frame String Env

data Config = Config {
    tracingEnabled :: Bool
}

