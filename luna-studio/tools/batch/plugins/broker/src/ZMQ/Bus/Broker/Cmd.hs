module ZMQ.Bus.Broker.Cmd where

import           Prologue



data Cmd = Serve { verbose :: Int
                 , noColor :: Bool
                 }
         | Version
         deriving Show
