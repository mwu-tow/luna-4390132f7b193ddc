{-# LANGUAGE Strict #-}

module Searcher.Engine.Metric.MismatchPenalty where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.Text                   as Text
import qualified Searcher.Engine.Data.Match       as Match

import Searcher.Engine.Data.Score (Score (Score))
import Searcher.Engine.Metric     (Metric (getMetric, updateMetric))
import Control.Lens (to)



-------------------------
-- === SkipPenalty === --
-------------------------

-- === Definition === --

data MismatchPenalty = MismatchPenalty
    { _mismatched :: Int
    , _multiplier :: Int
    } deriving (Generic, Show)
makeLenses ''MismatchPenalty


-- === Instances === --

instance Default MismatchPenalty where def = MismatchPenalty def $! -4

instance NFData  MismatchPenalty

instance Metric  MismatchPenalty where
    updateMetric _ charMatch matchState = let
        finished  = matchState ^. Match.remainingSuffix . to Text.null
        isMatched = charMatch == Match.Equal || finished
        in unless isMatched
            $! State.modify_ @MismatchPenalty $! mismatched %~ (+1)
    getMetric _ = do
        mult   <- State.use @MismatchPenalty multiplier
        points <- State.use @MismatchPenalty mismatched
        pure $! Score $! mult * points

