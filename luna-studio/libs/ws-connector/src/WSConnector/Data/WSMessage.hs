{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module WSConnector.Data.WSMessage where

import           Data.Binary
import           Data.ByteString (ByteString)
import           Prologue

data ControlCode = ConnectionTakeover
                 | Welcome
                 deriving (Show, Generic)

instance Binary ControlCode

data WSMessage = WebMessage { _topic   :: String
                            , _message :: ByteString
                            }
               | ControlMessage ControlCode
               deriving (Show, Generic)

makeLenses ''WSMessage
instance Binary WSMessage
