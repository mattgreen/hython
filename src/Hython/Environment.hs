module Hython.Environment
where

import Control.Monad.State

import Hython.Frame
import Hython.InterpreterState

currentScope :: Interpreter Scope
currentScope = do
    frame <- currentFrame
    return $ scopeOf frame

  where
    scopeOf (Frame _ s) = s

updateScope :: Scope -> Interpreter ()
updateScope scope = do
    Frame name _ : fs <- gets frames

    modify $ \s -> s { frames = Frame name scope : fs }
