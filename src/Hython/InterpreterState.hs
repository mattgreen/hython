module Hython.InterpreterState
where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Cont hiding (cont)

import Language.Python.Core

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

data Scope = Scope
    { localScope        :: AttributeDict
    , moduleScope       :: AttributeDict
    , builtinScope      :: AttributeDict
    , activeScope       :: ActiveScope
    } deriving (Show)

data ActiveScope
    = ModuleScope
    | LocalScope
    deriving (Eq, Show)

data Frame = Frame String Scope

data Config = Config {
    tracingEnabled :: Bool
}

