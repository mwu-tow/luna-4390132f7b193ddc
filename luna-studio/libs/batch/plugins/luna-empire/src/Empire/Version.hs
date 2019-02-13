module Empire.Version where

import           Prologue

import qualified Data.Version  as Version

import qualified Empire.Config as Config

fullVersion :: String
fullVersion = "Luna empire version " <> Version.showVersion Config.version
