module Hython.ControlFlow
where

import Safe (headMay, tailSafe)

import Hython.Types

new :: Flow m
new = Flow {
    flowBreakConts = [],
    flowContinueConts = [],
    flowReturnConts = []
}

getBreak :: MonadFlow m => m (Maybe (Continuation m))
getBreak = headMay . flowBreakConts <$> getFlow

getContinue :: MonadFlow m => m (Maybe (Continuation m))
getContinue = headMay . flowContinueConts <$> getFlow

getReturn :: MonadFlow m => m (Maybe (Continuation m))
getReturn = headMay . flowReturnConts <$> getFlow

popBreakCont :: MonadFlow m => m ()
popBreakCont = modifyFlow $ \flow ->
    flow { flowBreakConts = tailSafe $ flowBreakConts flow }

popContinueCont :: MonadFlow m => m ()
popContinueCont = modifyFlow $ \flow ->
    flow { flowContinueConts = tailSafe $ flowContinueConts flow }

popReturnCont :: MonadFlow m => m ()
popReturnCont = modifyFlow $ \flow ->
    flow { flowReturnConts = tailSafe $ flowReturnConts flow }

pushBreakCont :: MonadFlow m => (Object -> m ()) -> m ()
pushBreakCont cont = modifyFlow $ \flow ->
    flow { flowBreakConts = cont : flowBreakConts flow }

pushContinueCont :: MonadFlow m => (Object -> m ()) -> m ()
pushContinueCont cont = modifyFlow $ \flow ->
    flow { flowContinueConts = cont : flowContinueConts flow }

pushReturnCont :: MonadFlow m => (Object -> m ()) -> m ()
pushReturnCont cont = modifyFlow $ \flow ->
    flow { flowReturnConts = cont : flowReturnConts flow }
