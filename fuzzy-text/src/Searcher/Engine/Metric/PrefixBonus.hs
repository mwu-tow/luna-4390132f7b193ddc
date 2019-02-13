{-# LANGUAGE Strict #-}

module Searcher.Engine.Metric.PrefixBonus where

import Prologue

import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Substring as Substring
import qualified Searcher.Engine.Metric         as Metric

import Searcher.Engine.Data.Score (Score (Score))



-------------------------
-- === PrefixBonus === --
-------------------------

-- === Definition === --

data PrefixBonus = PrefixBonus
    { _multiplier :: Int
    } deriving (Generic, Show)
makeLenses ''PrefixBonus


-- === Instances === --

instance Default PrefixBonus where
    def = PrefixBonus 12
    {-# INLINE def #-}

instance NFData PrefixBonus

instance Metric.State PrefixBonus where
    updateMetric metricSt _ _ _ = metricSt
    {-# INLINE updateMetric #-}

    getMetric metricSt matchState = let
        substring      = matchState ^. Match.currentSubstring
        mayPrefixRange = head $! substring ^. Substring.range
        getPoints = \r -> let
            isPrefix = r ^. Substring.begin == 0
            rLen     = r ^. Substring.len + 1
            in if isPrefix then rLen * (rLen + 1) `quot` 2 else 0
        points  = maybe 0 getPoints mayPrefixRange
        mkScore = \m -> Score $! m * points
        multM   = metricSt ^. multiplier
        in mkScore multM
    {-# INLINE getMetric #-}

