module ZMQ.Bus.Broker.Proxy where

import           System.ZMQ4.Monadic (ZMQ)
import qualified System.ZMQ4.Monadic as ZMQ

import           Prologue
import           ZMQ.Bus.EndPoint    (EndPoint)


run :: EndPoint -> EndPoint -> IO ()
run pullAddr pubAddr = ZMQ.runZMQ $ serve pullAddr pubAddr


serve :: EndPoint -> EndPoint -> ZMQ z ()
serve pullAddr pubAddr = do
    pull <- ZMQ.socket ZMQ.Pull
    pub  <- ZMQ.socket ZMQ.Pub
    ZMQ.bind pull pullAddr
    ZMQ.bind pub pubAddr
    ZMQ.proxy pull pub Nothing
