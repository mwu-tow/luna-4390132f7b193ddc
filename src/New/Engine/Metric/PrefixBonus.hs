{-# LANGUAGE Strict #-}
module New.Engine.Metric.PrefixBonus where

import Prologue

import qualified Control.Monad.State.Layered as State

import New.Engine.Metric (Metric (updateMetric))
import New.Engine.Data.Score (Score (Score))
import Control.Lens (Getter, to)


data PrefixBonus = PrefixBonus
    { _prefixLength :: Int
    , _multiplier   :: Int
    , _interrupted  :: Bool
    } deriving (Generic, Show)

makeLenses ''PrefixBonus

bonus :: Getter PrefixBonus Score
bonus = to $! \m -> let
    points = m ^. prefixLength
    mult   = m ^. multiplier
    in Score $! points * mult

instance Default PrefixBonus where def = PrefixBonus def 12 False
instance NFData  PrefixBonus
instance Metric  PrefixBonus where
    updateMetric c1 c2 = let 
        interruptedM   = State.use @PrefixBonus interrupted
        prefixBonusM   = State.get @PrefixBonus 
        getBonus       = \pb -> pb ^. bonus
        getScore       = getBonus <$> prefixBonusM
        setInterrupted = State.modify_ @PrefixBonus $! (& interrupted  .~ True)
        addToPrefix    = State.modify_ @PrefixBonus $! (& prefixLength %~ (+1))
        in do
            unlessM interruptedM 
                $ if c1 == c2 then addToPrefix else setInterrupted
            getScore



