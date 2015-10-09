{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Hython.ControlFlow
where

import Safe (headMay, tailSafe)

class Monad m => MonadFlow cont m | m -> cont where
    getFlow         :: m (Flow cont)
    putFlow         :: Flow cont -> m ()
    modifyFlow      :: (Flow cont -> Flow cont) -> m ()
    modifyFlow f = do
        flow <- getFlow
        putFlow $ f flow

data Flow cont = Flow
    { flowBreakConts    :: [cont]
    , flowContinueConts :: [cont]
    , flowReturnConts   :: [cont]
    }

new :: Flow cont
new = Flow {
    flowBreakConts = [],
    flowContinueConts = [],
    flowReturnConts = []
}

getBreakCont :: MonadFlow cont m => m (Maybe cont)
getBreakCont = headMay . flowBreakConts <$> getFlow

getContinueCont :: MonadFlow cont m => m (Maybe cont)
getContinueCont = headMay . flowContinueConts <$> getFlow

getReturnCont :: MonadFlow cont m => m (Maybe cont)
getReturnCont = headMay . flowReturnConts <$> getFlow

popBreakCont :: MonadFlow cont m => m ()
popBreakCont = modifyFlow $ \flow ->
    flow { flowBreakConts = tailSafe $ flowBreakConts flow }

popContinueCont :: MonadFlow cont m => m ()
popContinueCont = modifyFlow $ \flow ->
    flow { flowContinueConts = tailSafe $ flowContinueConts flow }

popReturnCont :: MonadFlow cont m => m ()
popReturnCont = modifyFlow $ \flow ->
    flow { flowReturnConts = tailSafe $ flowReturnConts flow }

pushBreakCont :: MonadFlow cont m => cont -> m ()
pushBreakCont cont = modifyFlow $ \flow ->
    flow { flowBreakConts = cont : flowBreakConts flow }

pushContinueCont :: MonadFlow cont m => cont -> m ()
pushContinueCont cont = modifyFlow $ \flow ->
    flow { flowContinueConts = cont : flowContinueConts flow }

pushReturnCont :: MonadFlow cont m => cont -> m ()
pushReturnCont cont = modifyFlow $ \flow ->
    flow { flowReturnConts = cont : flowReturnConts flow }
