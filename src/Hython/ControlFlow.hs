module Hython.ControlFlow
where

import Safe (headMay, tailSafe)

data ControlFlow m = ControlFlow
    { flowBreakConts    :: [m ()]
    , flowContinueConts :: [m ()]
    }

new :: ControlFlow m
new = ControlFlow {
    flowBreakConts = [],
    flowContinueConts = []
}

getBreak :: ControlFlow m -> Maybe (m ())
getBreak flow = headMay . flowBreakConts $ flow

getContinue :: ControlFlow m -> Maybe (m ())
getContinue flow = headMay . flowContinueConts $ flow

popBreak :: ControlFlow m -> ControlFlow m
popBreak flow = flow { flowBreakConts = tailSafe $ flowBreakConts flow }

popContinue :: ControlFlow m -> ControlFlow m
popContinue flow = flow { flowContinueConts = tailSafe $ flowContinueConts flow }

pushBreak :: m () -> ControlFlow m -> ControlFlow m
pushBreak cont flow = flow { flowBreakConts = cont : flowBreakConts flow }

pushContinue :: m () -> ControlFlow m -> ControlFlow m
pushContinue cont flow = flow { flowContinueConts = cont : flowContinueConts flow }
