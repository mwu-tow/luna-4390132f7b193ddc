{-# LANGUAGE Strict #-}
module New.Engine.Metric.PrefixBonus where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Match     as Match
import qualified New.Engine.Data.Substring as Substring

import New.Engine.Metric (Metric (updateMetric))
import New.Engine.Data.Score (Score (Score))


data PrefixBonus = PrefixBonus
    { _multiplier :: Int
    } deriving (Generic, Show)

makeLenses ''PrefixBonus


instance Default PrefixBonus where def = PrefixBonus 12
instance NFData  PrefixBonus
instance Metric  PrefixBonus where
    updateMetric _ _ updatedState = let
        substring      = updatedState ^. Match.currentSubstring
        mayPrefixRange = head $! substring ^. Substring.range
        getPoints      = \r -> let rLen = r ^. Substring.len + 1
            in rLen * (rLen + 1) `quot` 2
        points         = maybe 0 getPoints mayPrefixRange
        mkScore        = \m -> Score $! m * points
        multM          = State.use @PrefixBonus multiplier
        in mkScore <$> multM
