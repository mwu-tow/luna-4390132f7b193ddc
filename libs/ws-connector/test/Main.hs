module Main where

import qualified WSConnector.WSConnector   as WSConnector
import           ZMQ.Bus.Bus               (Bus)
import qualified ZMQ.Bus.Bus               as Bus
import qualified ZMQ.Bus.Data.Flag         as Flag
import qualified ZMQ.Bus.Data.Message      as Message
import qualified ZMQ.Bus.Data.MessageFrame as MessageFrame
import           ZMQ.Bus.EndPoint          (BusEndPoints (BusEndPoints))

import qualified Data.ByteString.Char8     as Char8
import           Prologue
import qualified System.ZMQ4.Monadic       as ZMQ



main :: IO ()
main = WSConnector.run
