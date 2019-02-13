module ZMQ.Bus.Logger.Cmd where

import           Prologue



data Cmd = Run { topics  :: [String]

               , verbose :: Int
               , noColor :: Bool
               }
         | Version
         deriving Show
