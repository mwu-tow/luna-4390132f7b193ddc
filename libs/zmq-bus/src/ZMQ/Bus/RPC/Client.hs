{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZMQ.Bus.RPC.Client where

import           Control.Monad             (liftM)
import           Data.Binary               (Binary, decode, encode)
import           Data.ByteString.Lazy      (fromStrict, toStrict)

import           Prologue
import           System.Log.MLogger
import           ZMQ.Bus.Bus               (Bus)
import qualified ZMQ.Bus.Bus               as Bus
import qualified ZMQ.Bus.Data.Flag         as Flag
import           ZMQ.Bus.Data.Message      (Message (Message))
import qualified ZMQ.Bus.Data.Message      as Message
import           ZMQ.Bus.Data.MessageFrame (MessageFrame)
import qualified ZMQ.Bus.Data.MessageFrame as MessageFrame
import           ZMQ.Bus.Data.Topic        (Topic)
import qualified ZMQ.Bus.Data.Topic        as Topic
import           ZMQ.Bus.RPC.Types



logger :: Logger
logger = getLogger $moduleName


isCorrelationIDValid :: Message.CorrelationID -> MessageFrame -> Bool
isCorrelationIDValid correlationID frame =
    frame ^. MessageFrame.correlation == correlationID


isRequest :: MessageFrame -> Bool
isRequest frame =
    Topic.isRequest $ frame ^. MessageFrame.message . Message.topic


allFramesReceived :: Message.CorrelationID -> MessageFrame -> Bool
allFramesReceived correlationID frame =
    not (isRequest frame)
    && isCorrelationIDValid correlationID frame
    && frame ^. MessageFrame.lastFrame == Flag.Enable


query :: (Binary args, Typeable args, Binary result, Typeable result)
      => String -> Topic -> args -> Bus [result]
query pluginName topic args = do
    results <- queryRaw $ Message (pluginName <> "." <> topic) $ toStrict . encode $ Request topic $ packValue args
    mapM (lift . unpackValue . retVal . result . decode . fromStrict .view Message.message) results


queryRaw :: Message -> Bus [Message]
queryRaw message = do
    let topicBase = Topic.base $ message ^. Message.topic
    logger debug "Query : sending..."
    Bus.subscribe topicBase
    correlationID <- Bus.send Flag.Enable message
    logger debug "Query : receiving responses..."
    frames <- repeatUntil Bus.receive (not . allFramesReceived correlationID)
    Bus.unsubscribe topicBase
    logger debug "Query : complete"
    return $ map (view MessageFrame.message)
           $ filter (not . isRequest)
           $ filter (isCorrelationIDValid correlationID) frames

repeatUntil :: Monad m => m a -> (a -> Bool) -> m [a]
repeatUntil action predicate = do
   result <- action
   if predicate result
       then liftM ((:) result) (repeatUntil action predicate)
       else return [result]
