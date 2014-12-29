module Hython.Environment
where

import Control.Monad.State

import Hython.Frame
import Hython.InterpreterState
import Hython.Object

currentEnv :: Interpreter Env
currentEnv = do
    frame <- currentFrame
    return $ envOf frame

  where
    envOf (Frame _ e) = e

updateEnv :: Env -> Interpreter ()
updateEnv env = do
    Frame name _ : fs <- gets frames

    modify $ \s -> s { frames = Frame name env : fs }
