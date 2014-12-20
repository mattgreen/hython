module Hython.Frame
where

import Control.Monad.State

import Hython.InterpreterState

currentFrame :: Interpreter Frame
currentFrame = do
    currentFrames <- gets frames
    return $ head currentFrames

pushFrame :: String -> Scope -> Interpreter ()
pushFrame context scope = modify $ \s -> s { frames = newFrame : frames s }
  where
    newFrame = Frame context scope

popFrame :: Interpreter ()
popFrame = do
    currentFrames <- gets frames
    case currentFrames of
        []      -> return ()
        (_:fs)  -> modify $ \s -> s { frames = fs }
