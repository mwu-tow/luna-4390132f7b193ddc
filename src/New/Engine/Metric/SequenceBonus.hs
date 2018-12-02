{-# LANGUAGE Strict #-}

module New.Engine.Metric.SequenceBonus where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Match     as Match
import qualified New.Engine.Data.Substring as Substring

import New.Engine.Metric (Metric (updateMetric, getMetric))
import New.Engine.Data.Score (Score (Score))



---------------------------
-- === SequenceBonus === --
---------------------------

-- === Definition === --

data SequenceBonus = SequenceBonus
    { _multiplier :: Int
    } deriving (Generic, Show)
makeLenses ''SequenceBonus


-- === Instances === --

instance Default SequenceBonus where def = SequenceBonus 10

instance NFData  SequenceBonus

instance Metric  SequenceBonus where
    updateMetric _ _ updatedState = undefined --let
        -- revRange
            -- = updatedState ^. Match.currentSubstring . Substring.reversedRange
        -- getRangeScore = \r -> let
            -- rLen = r ^. Substring.len
            -- rLenPred = rLen - 1
            -- points   = rLen * rLenPred `quot` 2
            -- in max 0 points
        -- points      = foldl (\acc r -> acc + getRangeScore r) def revRange
        -- mkScore     = \m -> Score $! m * points
        -- multM       = State.use @SequenceBonus multiplier
        -- in mkScore <$> multM
    getMetric = undefined

