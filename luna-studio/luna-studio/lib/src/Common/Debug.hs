{-# LANGUAGE CPP #-}
module Common.Debug where

import           Common.Action.Command
import           Common.Prelude
import           Data.Map                (Map)
import           Data.Time.Clock         (UTCTime)
import           Data.UUID.Types         (UUID)
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as Topic
#ifdef DEBUG_PERF
import qualified Data.Map                as Map
import           Data.Time.Clock         (diffUTCTime, getCurrentTime)
#endif

class HasRequestTimes st where
    requestTimes :: Lens' st (Map UUID UTCTime)

measureResponseTime :: (HasRequestTimes s, Topic.MessageTopic (Response.Response req inv res)) =>
                       Response.Response req inv res -> Command s ()
#ifdef DEBUG_PERF
measureResponseTime resp = do
    let uuid = resp ^. Response.requestId
    reqTimeM <- use $ requestTimes . at uuid
    liftIO $ case reqTimeM of
        Just reqTime -> do
            currTime <- getCurrentTime
            let timeDiff = show $ diffUTCTime currTime reqTime
            putStrLn $ "[Request time -- NodeEditor] " <> Topic.topic resp <> " took " <> timeDiff
        Nothing      -> putStrLn $ "[Request time -- NodeEditor] request uuid doesn't match any known requests."
#else
measureResponseTime _ = return ()
#endif
