module Hython.ControlFlow
where

import Safe (headMay, tailSafe)

data ControlFlow cont = ControlFlow
    { flowBreakConts    :: [cont]
    , flowContinueConts :: [cont]
    , flowReturnConts   :: [cont]
    }

data Frame cont = Frame
    { frameBreakConts       :: [cont]
    , frameContinueConts    :: [cont]
    , frameReturnCont       :: [cont]
    }

new :: ControlFlow cont
new = ControlFlow {
    flowBreakConts = [],
    flowContinueConts = [],
    flowReturnConts = []
}

getBreak :: ControlFlow cont -> Maybe cont
getBreak flow = headMay . flowBreakConts $ flow

getContinue :: ControlFlow cont -> Maybe cont
getContinue flow = headMay . flowContinueConts $ flow

getReturn :: ControlFlow cont -> Maybe cont
getReturn flow = headMay . flowReturnConts $ flow

popBreak :: ControlFlow cont -> ControlFlow cont
popBreak flow = flow { flowBreakConts = tailSafe $ flowBreakConts flow }

popContinue :: ControlFlow cont -> ControlFlow cont
popContinue flow = flow { flowContinueConts = tailSafe $ flowContinueConts flow }

popReturn :: ControlFlow cont -> ControlFlow cont
popReturn flow = flow { flowReturnConts = tailSafe $ flowReturnConts flow }

pushBreak :: cont -> ControlFlow cont -> ControlFlow cont
pushBreak cont flow = flow { flowBreakConts = cont : flowBreakConts flow }

pushContinue :: cont -> ControlFlow cont -> ControlFlow cont
pushContinue cont flow = flow { flowContinueConts = cont : flowContinueConts flow }

pushReturn :: cont -> ControlFlow cont -> ControlFlow cont
pushReturn cont flow = flow { flowReturnConts = cont : flowReturnConts flow }
