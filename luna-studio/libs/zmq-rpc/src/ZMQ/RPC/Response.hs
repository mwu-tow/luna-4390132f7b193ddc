{-# LANGUAGE DeriveGeneric #-}
module ZMQ.RPC.Response where

import           Data.Binary     (Binary)
import           Prologue


data Response a = Result    { requestID :: Int, result :: a}
                | Exception { requestID :: Int, msg :: String }
                deriving (Show, Generic)

instance Binary a => Binary (Response a)
