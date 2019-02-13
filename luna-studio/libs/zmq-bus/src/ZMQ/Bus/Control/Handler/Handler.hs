{-# LANGUAGE RankNTypes #-}

module ZMQ.Bus.Control.Handler.Handler where

import           ZMQ.Bus.Control.BusCtx          (BusCtx)
import           ZMQ.Bus.Control.Handler.ID      as HandlerID
import           ZMQ.Bus.Control.Handler.Methods
import           ZMQ.RPC.Handler                 (RPCHandler)


handler :: BusCtx -> RPCHandler Methods Results
handler ctx callback request = case request of
    CreateID {} -> callback (HandlerID.create ctx)
