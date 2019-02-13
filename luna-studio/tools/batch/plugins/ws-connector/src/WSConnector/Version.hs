module WSConnector.Version where

import qualified Data.Version               as Version
import           Prologue
import qualified WSConnector.Config as Config

full :: Bool -> String
full numeric = prefix <> Version.showVersion Config.version where
    prefix = if numeric
             then ""
             else "ZMQ bus websocket connector version "
