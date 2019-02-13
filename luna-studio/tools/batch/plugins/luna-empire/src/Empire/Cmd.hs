module Empire.Cmd where

import Prologue


data Cmd = Run { topics      :: [String]
               , verbose     :: Int
               , formatted   :: Bool
               }
         | Version
         deriving Show

