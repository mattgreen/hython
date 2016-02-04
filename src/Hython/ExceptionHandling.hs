module Hython.ExceptionHandling
where

import Control.Monad.Cont.Class (MonadCont)
import Data.Text (Text)

import Hython.Call (call)
import Hython.ControlFlow (getExceptionHandler, setCurrentException)
import Hython.Environment (MonadEnv, lookupName)
import Hython.Types (MonadInterpreter, newString, Object)
import qualified Hython.Types as Types

raise :: MonadInterpreter m => Object -> m ()
raise exception = do
    handler <- getExceptionHandler

    setCurrentException exception
    handler exception

raiseInternal :: (MonadCont m, MonadEnv Object m, MonadInterpreter m) => Text -> Text -> m ()
raiseInternal clsName description = do
    mcls <- lookupName clsName
    case mcls of
        Just obj    -> do
            descriptionStr  <- newString description
            exception       <- call obj [descriptionStr] []
            raise exception
        _           -> Types.raise "TypeError" "exceptions must derive from BaseException"



