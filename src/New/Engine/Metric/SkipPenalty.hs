{-# LANGUAGE Strict #-}
module New.Engine.Metric.SkipPenalty where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Match       as Match
import qualified New.Engine.Data.Substring   as Substring

import New.Engine.Data.Score (Score (Score))
import New.Engine.Metric     (Metric (updateMetric))


data SkipPenalty = SkipPenalty
    { _multiplier     :: Int
    } deriving (Generic, Show)

makeLenses ''SkipPenalty

instance Default SkipPenalty where def = SkipPenalty $! -4
instance NFData  SkipPenalty
instance Metric  SkipPenalty where
    updateMetric _ _ updatedState = let
        posInData     = updatedState ^. Match.positionInData
        substring     = updatedState ^. Match.currentSubstring
        matchedLen    = substring ^. Substring.totalLength
        missedLetters = posInData - matchedLen
        mkScore       = \m -> Score $! m * missedLetters
        multM         = State.use @SkipPenalty multiplier
        in mkScore <$> multM
