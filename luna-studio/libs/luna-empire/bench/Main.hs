{-# LANGUAGE BangPatterns #-}

module Main where

import Empire.Prelude

import Criterion.Main
import Criterion.Types

import qualified Empire.Commands.Graph as Graph
import           Empire.Empire

import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TChan

instance NFData CommunicationEnv

instance NFData (TChan a) where
    rnf !_ = ()

main :: IO ()
main = do
    let makeEnv = CommunicationEnv <$> newTChanIO <*> newEmptyMVar <*> newEmptyMVar
        path    = "../../luna-studio/atom/test.luna"
        config  = defaultConfig { timeLimit = 30 }
    defaultMainWith config [
          env makeEnv $ \comm -> bench "openFile" $ nfIO $ execEmpire comm def (Graph.openFile path)
        ]
