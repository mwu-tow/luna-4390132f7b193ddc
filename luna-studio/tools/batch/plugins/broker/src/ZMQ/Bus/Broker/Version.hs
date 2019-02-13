module ZMQ.Bus.Broker.Version where

import qualified Data.Version          as Version

import           Prologue
import qualified ZMQ.Bus.Broker.Config as Config



full :: Bool -> String
full = broker


broker :: Bool -> String
broker numeric = (if numeric then "" else "ZMQ bus broker version ") <> Version.showVersion Config.version
