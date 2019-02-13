module WSConnector.Cmd where

import           Prologue

data Cmd = Run { verbose :: Int }
         | Version
         deriving Show
