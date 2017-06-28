{-# LANGUAGE DeriveGeneric #-}

module ZMQ.Bus.Control.Handler.Methods where

import           Data.Binary     (Binary)

import           Prologue


data Methods = CreateID
             deriving (Show, Eq, Generic)

data Results = CreateIDResult { clientID :: Int }
             deriving (Show, Eq, Generic)

instance Binary Methods
instance Binary Results
