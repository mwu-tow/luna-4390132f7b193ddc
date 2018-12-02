{-# LANGUAGE Strict    #-}
module New.Engine.Metric.SkipPenalty where

import Prologue

import qualified Control.Monad.State.Layered as State

import New.Engine.Metric (Metric (updateMetric))
import New.Engine.Data.Score (Score (Score))
import Control.Lens (Getter, to)
import Data.Char (toLower)


data SkipPenalty = SkipPenalty
    { _skippedLetters :: Int
    , _multiplier     :: Int
    } deriving (Generic, Show)

makeLenses ''SkipPenalty

penalty :: Getter SkipPenalty Score
penalty = to $! \m -> let
    points = m ^. skippedLetters
    mult   = m ^. multiplier
    in Score $! points * mult

instance Default SkipPenalty where def = SkipPenalty def (-4)
instance NFData SkipPenalty
instance Metric SkipPenalty where
    updateMetric c1 c2 = let 
        updateState    = State.modify_ @SkipPenalty (& skippedLetters %~ (+1))
        charsEqual     = toLower c1 == toLower c2
        updateIfDiffer = unless charsEqual updateState
        skipPenaltyM   = State.get @SkipPenalty
        penaltyM       = view penalty <$> skipPenaltyM
        in updateIfDiffer >> penaltyM



