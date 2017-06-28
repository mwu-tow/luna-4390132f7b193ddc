{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module ZMQ.RPC.Handler where

import           Data.Binary         (Binary)
import           Prologue
import           System.ZMQ4.Monadic (ZMQ)

import           ZMQ.RPC.Response    (Response)
import           ZMQ.RPC.RPC         (RPC)


type Serializable a b = (Binary a, Binary b, Show b)

type RPCHandler request result = forall z .
        (((request -> RPC result) -> ZMQ z (Response result))
        -> request -> ZMQ z (Response result))
