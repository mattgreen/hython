module Hython.Environment
where

import Control.Monad.State

import Hython.InterpreterState

currentFrame :: Interpreter Frame
currentFrame = do
    currentFrames <- gets frames
    return $ head currentFrames

currentScope :: Interpreter Scope
currentScope = do
    frame <- currentFrame
    return $ scopeOf frame

  where
    scopeOf (Frame _ s) = s

updateScope :: Scope -> Interpreter ()
updateScope scope = do
    Frame name _ : fs <- gets frames

    modify $ \e -> e { frames = Frame name scope : fs }

