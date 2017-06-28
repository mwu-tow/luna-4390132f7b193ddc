{-# LANGUAGE TemplateHaskell #-}

module WSConnector.WSConfig where

import           Prologue
import qualified ZMQ.Bus.WS.Config as FD

data Config = Config { _host     :: String
                     , _port     :: Int
                     , _pingTime :: Int
                     } deriving (Read, Show, Eq)

makeLenses ''Config

readWebsocketConfig config = Config host port pingTime where
    host       = FD.host websocket
    port       = read (FD.port websocket) :: Int
    pingTime   = read (FD.pingTime websocket) :: Int
    websocket  = FD.websocket config
