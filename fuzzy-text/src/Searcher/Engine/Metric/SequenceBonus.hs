{-# LANGUAGE Strict #-}

module Searcher.Engine.Metric.SequenceBonus where

import Prologue

import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Substring as Substring
import qualified Searcher.Engine.Metric         as Metric

import Searcher.Engine.Data.Score (Score (Score))



---------------------------
-- === SequenceBonus === --
---------------------------

-- === Definition === --

data SequenceBonus = SequenceBonus
    { _multiplier :: Int
    } deriving (Generic, Show)
makeLenses ''SequenceBonus


-- === Instances === --

instance Default SequenceBonus where
    def = SequenceBonus 10
    {-# INLINE def #-}

instance NFData SequenceBonus

instance Metric.State SequenceBonus where
    updateMetric metricSt _ _ _ = metricSt -- pure ()
    {-# INLINE updateMetric #-}

    getMetric metricSt matchState = let
        revRange
            = matchState ^. Match.currentSubstring . Substring.reversedRange
        getRangeScore = \r -> let
            rLen     = r ^. Substring.len
            rLenPred = rLen - 1
            pts      = rLen * rLenPred `quot` 2
            in max 0 pts
        points      = foldl (\acc r -> acc + getRangeScore r) def revRange
        mkScore     = \m -> Score $! m * points
        multM       = metricSt ^. multiplier
        in mkScore multM
    {-# INLINE getMetric #-}

