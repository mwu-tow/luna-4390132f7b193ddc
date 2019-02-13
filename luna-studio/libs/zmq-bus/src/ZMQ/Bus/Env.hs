module ZMQ.Bus.Env where

import qualified System.ZMQ4.Monadic  as ZMQ

import qualified ZMQ.Bus.Data.Message as Message



data BusEnv z = BusEnv { subSocket  :: ZMQ.Socket z ZMQ.Sub
                       , pushSocket :: ZMQ.Socket z ZMQ.Push
                       , clientID   :: Message.ClientID
                       , requestID  :: Message.RequestID
                       }
