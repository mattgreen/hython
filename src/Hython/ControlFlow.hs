{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Hython.ControlFlow
where

import Control.Monad (when)
import Safe (tailSafe)

class Monad m => MonadFlow obj cont m | m -> obj cont where
    getFlow         :: m (Flow obj cont)
    putFlow         :: Flow obj cont -> m ()
    modifyFlow      :: (Flow obj cont -> Flow obj cont) -> m ()
    modifyFlow f = do
        flow <- getFlow
        putFlow $ f flow

data Flow obj cont = Flow
    { flowDefaultBreak      :: cont
    , flowDefaultContinue   :: cont
    , flowDefaultReturn     :: cont
    , flowDefaultExcept     :: cont
    , flowCurrentException  :: Maybe obj
    , flowFrames            :: [Frame cont]
    , flowExcept            :: cont
    }

data Frame cont = Frame
    { frameBreak        :: cont
    , frameContinue     :: cont
    , frameReturn       :: cont
    }

new :: cont -> cont -> cont -> cont -> Flow obj cont
new defaultBreak defaultContinue defaultReturn defaultExcept = Flow
    { flowDefaultBreak = defaultBreak
    , flowDefaultContinue = defaultContinue
    , flowDefaultReturn = defaultReturn
    , flowDefaultExcept = defaultExcept
    , flowCurrentException = Nothing
    , flowFrames = [newFrame defaultBreak defaultContinue defaultReturn]
    , flowExcept = defaultExcept
    }

newFrame :: cont -> cont -> cont -> Frame cont
newFrame brk cont rtn = Frame
     { frameBreak = brk
     , frameContinue = cont
     , frameReturn = rtn
     }

currentFrame :: MonadFlow obj cont m => m (Frame cont)
currentFrame = head . flowFrames <$> getFlow

modifyCurrentFrame :: MonadFlow obj cont m => (Frame cont -> Frame cont) -> m ()
modifyCurrentFrame action = do
    frame <- currentFrame
    modifyFlow $ \f -> f { flowFrames = action frame : tailSafe (flowFrames f) }

clearCurrentException :: MonadFlow obj cont m => m ()
clearCurrentException = modifyFlow $ \f ->
    f { flowCurrentException = Nothing }

getBreakHandler :: MonadFlow obj cont m => m cont
getBreakHandler = frameBreak <$> currentFrame

getContinueHandler :: MonadFlow obj cont m => m cont
getContinueHandler = frameContinue <$> currentFrame

getCurrentException :: MonadFlow obj cont m => m (Maybe obj)
getCurrentException = flowCurrentException <$> getFlow

getExceptionHandler :: MonadFlow obj cont m => m cont
getExceptionHandler = flowExcept <$> getFlow

getFrameDepth :: MonadFlow obj cont m => m Int
getFrameDepth = length . flowFrames <$> getFlow

getReturnHandler :: MonadFlow obj cont m => m cont
getReturnHandler = frameReturn <$> currentFrame

popFrame :: MonadFlow obj cont m => m ()
popFrame = do
    flow <- getFlow
    case flowFrames flow of
        (_:f:fs)    -> putFlow $ flow { flowFrames = f:fs }
        _           -> return ()

popFramesTo :: MonadFlow obj cont m => Int -> m ()
popFramesTo depth = do
    flow <- getFlow
    when (depth > (length . flowFrames $ flow)) $ do
        popFrame
        popFramesTo (depth - 1)

pushFrame :: MonadFlow obj cont m => cont -> m ()
pushFrame rtn = do
    flow    <- getFlow
    frame   <- pure $ newFrame (flowDefaultBreak flow) (flowDefaultContinue flow) rtn
    modifyFlow $ \f -> f { flowFrames = frame : flowFrames f }

setBreakHandler :: MonadFlow obj cont m => cont -> m ()
setBreakHandler handler = modifyCurrentFrame $ \f ->
    f { frameBreak = handler }

setContinueHandler :: MonadFlow obj cont m => cont -> m ()
setContinueHandler handler = modifyCurrentFrame $ \f ->
    f { frameContinue = handler }

setCurrentException :: MonadFlow obj cont m => obj -> m ()
setCurrentException e = modifyFlow $ \f ->
    f { flowCurrentException = Just e }

setExceptionHandler :: MonadFlow obj cont m => cont -> m ()
setExceptionHandler handler = modifyFlow $ \f ->
    f { flowExcept = handler }

setReturnHandler :: MonadFlow obj cont m => cont -> m ()
setReturnHandler handler = modifyCurrentFrame $ \f ->
    f { frameReturn = handler }
