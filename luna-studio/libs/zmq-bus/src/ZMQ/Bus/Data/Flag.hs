module ZMQ.Bus.Data.Flag where

import           Text.Read       as R

import           Prologue



data Flag = Enable
          | Disable
          deriving (Eq, Ord)


instance Show Flag where
    show Enable  = show (1 :: Int)
    show Disable = show (0 :: Int)


instance Read Flag where
    readPrec = do n <- readPrec
                  case n :: Int of
                      1 -> return Enable
                      0 -> return Disable
                      _ -> R.pfail
