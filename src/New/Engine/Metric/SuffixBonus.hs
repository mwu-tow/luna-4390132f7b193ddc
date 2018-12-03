{-# LANGUAGE Strict #-}

module New.Engine.Metric.SuffixBonus where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Match       as Match
import qualified New.Engine.Data.Substring   as Substring

import New.Engine.Data.Score (Score (Score))
import New.Engine.Metric     (Metric (getMetric, updateMetric))



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
    updateMetric _ _ _ = pure ()
    getMetric matchState = let
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
        multM       = State.use @SuffixBonus multiplier
        in mkScore <$> multM

