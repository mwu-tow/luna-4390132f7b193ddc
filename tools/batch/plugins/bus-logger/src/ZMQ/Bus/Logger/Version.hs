module ZMQ.Bus.Logger.Version where

import qualified Data.Version              as Version

import qualified ZMQ.Bus.Logger.Config as Config
import           Prologue



full :: Bool -> String
full = logger


logger :: Bool -> String
logger numeric = (if numeric then "" else "ZMQ bus logger version ") <> Version.showVersion Config.version
