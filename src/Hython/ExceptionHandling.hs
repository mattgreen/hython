module Hython.ExceptionHandling
where

import Control.Monad.Cont.Class (MonadCont)
import Data.Text (Text)
import qualified Data.Text as T

import Hython.Call (call)
import Hython.Class (isSubClass)
import Hython.ControlFlow (getExceptionHandler, setCurrentException)
import Hython.Environment (MonadEnv, lookupName)
import Hython.Types hiding (raise)
import qualified Hython.Types as Types


raiseOr :: (MonadEnv Object m, MonadInterpreter m) => m () -> Object -> m ()
raiseOr raiseError exc@(Object objectInfo) = do
    mBaseClass <- lookupName (T.pack "BaseException")
    case mBaseClass of
        Just (Class baseClassInfo) ->
            if isSubClass (objectClass objectInfo) baseClassInfo
                then do
                    handler <- getExceptionHandler
                    setCurrentException exc
                    handler exc
                else raiseError
        _ -> Types.raise "SystemError" "could not find BaseException class"
raiseOr raiseError _ = raiseError

raiseExternal :: (MonadEnv Object m, MonadInterpreter m) => Object -> m ()
raiseExternal =
    raiseOr $ Types.raise "TypeError" "exceptions must derive from BaseException"

raiseInternal :: (MonadCont m, MonadEnv Object m, MonadInterpreter m) => Text -> Text -> m ()
raiseInternal clsName description = do
    mcls <- lookupName clsName
    case mcls of
        Just cls@(Types.Class _) -> do
            descriptionStr  <- newString description
            exception       <- call cls [descriptionStr] []
            raiseOr raiseError exception
        _ -> raiseError
  where
    raiseError = Types.raise "SystemError" ("internally raised invalid exception "
                     ++ T.unpack clsName ++ "(\"" ++ T.unpack description ++ "\")")

