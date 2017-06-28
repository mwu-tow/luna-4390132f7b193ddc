{-# LANGUAGE TemplateHaskell #-}

module ZMQ.Bus.Logger.Env where

import           Data.Map.Strict      (Map)
import           Data.Time.Clock      (UTCTime)

import           Prologue
import qualified ZMQ.Bus.Data.Message as Message



data Env = Env { _times :: !(Map Message.CorrelationID UTCTime) }
               deriving (Show)

makeLenses ''Env


instance Default Env where
    def = Env def
