module TextEditor.Handler.Backend.Common
    ( whenOk
    , handleResponse
    , doNothing
    ) where

import           Common.Action.Command   (Command)
import           Common.Debug            (measureResponseTime)
import           Common.Prelude
import           Common.Report           (error)
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as Topic
import           TextEditor.Action.UUID  (isOwnRequest, unregisterRequest)
import           TextEditor.State.Global (State)


whenOk :: Response.Response req inv res -> (res -> Command State ()) -> Command State ()
whenOk (Response.Response _ _ _ _ (Response.Ok    res)) handler = handler res
whenOk (Response.Response _ _ _ _ (Response.Error _  )) _       = return ()

handleResponse :: (Topic.MessageTopic (Response.Response req inv res), Show req) => Response.Response req inv res -> (res -> Command State ()) -> (Response.Status inv -> Command State ()) -> Command State ()
handleResponse resp@(Response.Response uuid _ _ inv res) success failure = do
    case res of
        Response.Ok    res' -> success res'
        Response.Error str  -> do
            error $ str <> "\n\nwhile processing event\n\n" <> Topic.topic resp
            failure inv
    measureResponseTime resp
    whenM (isOwnRequest uuid) $ unregisterRequest uuid

doNothing :: a -> Command State ()
doNothing = const $ return ()
