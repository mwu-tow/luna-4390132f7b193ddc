{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module ZMQ.Bus.Data.Message where

import           Data.ByteString    (ByteString)

import           Prologue
import           ZMQ.Bus.Data.Topic (Topic, (/+))
import qualified ZMQ.Bus.Data.Topic as Topic
import qualified ZMQ.Bus.RPC.RPC    as RPC
import           ZMQ.Bus.RPC.Types




type ID = Int

type RequestID = ID
type ClientID  = ID


data CorrelationID = CorrelationID { _clientID  :: ClientID
                                   , _messageID :: RequestID
                                   } deriving (Read, Show, Eq, Ord)


data Message = Message { _topic   :: Topic
                       , _message :: ByteString
                       } deriving (Read, Show, Eq)


makeLenses(''CorrelationID)
makeLenses(''Message)


instance Default CorrelationID where
    def = CorrelationID def def


mk :: Topic -> Response -> Message
mk topic' data_ = Message topic' $ RPC.messagePut' data_ where


mkError :: Topic -> FunctionName -> String -> [Message]
mkError topic' functionName = return . mk (topic' /+ Topic.error) . (response :: String -> Response)
	where
		response description = Response functionName (ErrorResult description) []
