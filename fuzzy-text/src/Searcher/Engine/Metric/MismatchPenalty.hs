{-# LANGUAGE Strict #-}

module Searcher.Engine.Metric.MismatchPenalty where

import Prologue

import qualified Data.Text                   as Text
import qualified Searcher.Engine.Data.Match  as Match
import qualified Searcher.Engine.Metric      as Metric

import Control.Lens               (to)
import Searcher.Engine.Data.Score (Score (Score))



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

instance Default MismatchPenalty where
    def = MismatchPenalty def $! -4
    {-# INLINE def #-}

instance NFData MismatchPenalty

instance Metric.State MismatchPenalty where
    updateMetric metricSt _ charMatch matchState = let
        finished  = matchState ^. Match.remainingSuffix . to Text.null
        isMatched = charMatch == Match.Equal || finished
        in if isMatched
            then metricSt
            else metricSt & mismatched %~ (+1)
    {-# INLINE updateMetric #-}

    getMetric metricSt _ =
        Score $! (metricSt ^. multiplier) * (metricSt ^. mismatched)
    {-# INLINE getMetric #-}


