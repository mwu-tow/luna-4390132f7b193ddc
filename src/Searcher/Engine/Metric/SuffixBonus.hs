{-# LANGUAGE Strict #-}

module Searcher.Engine.Metric.SuffixBonus where

import Prologue

import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Substring as Substring

import Searcher.Engine.Data.Score (Score (Score))
import Searcher.Engine.Metric     (Metric (getMetric, updateMetric))



-------------------------
-- === SuffixBonus === --
-------------------------

-- === Definition === --

data SuffixBonus = SuffixBonus
    { _multiplier :: Int
    } deriving (Generic, Show)
makeLenses ''SuffixBonus


-- === Instances === --

instance Default SuffixBonus where def = SuffixBonus 4

instance NFData  SuffixBonus

instance Metric  SuffixBonus where
    updateMetric metricSt _ _ _ = metricSt

    getMetric metricSt matchState = let
        posInData   = matchState ^. Match.positionInData
        substring   = matchState ^. Match.currentSubstring
        revRange    = substring ^. Substring.reversedRange
        mayRange    = head revRange
        points      = maybe 0 getPoints mayRange
        getPoints   = \r -> let
            rEnd    = r ^. Substring.end
            rLen    = r ^. Substring.len
            points' = rLen * (rLen + 1) `quot` 2
            in if rEnd == posInData then points' else 0
        mkScore     = \m -> Score $! m * points
        multM       = metricSt ^. multiplier
        in mkScore multM

