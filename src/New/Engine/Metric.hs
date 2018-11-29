{-# LANGUAGE Strict    #-}
{-# LANGUAGE PolyKinds #-}

module New.Engine.Metric where

import Prologue hiding (Monad)

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Score       as Score

import New.Engine.Data.Score (Score)



--------------------
-- === Metric === --
--------------------

-- === Definition === --


-- === API === --


-- === Instances === --

class (Default s, NFData s) => Metric s where
    updateMetric :: forall m . State.Monad s m => Text -> Text -> m Score

type Monad s m        = (Metric s, State.Monad s m)
type MonadMetrics s m = (Metrics s, State.MonadStates s m)

type family Metrics ss :: Constraint where
    Metrics (s ': ss) = (Metric s, Metrics ss)
    Metrics '[]       = ()

runSomeFn2 :: Score
runSomeFn2 = State.evalDef @DummyState someFn2

someFn2 :: Monad DummyState m => m Score
someFn2 = do
    newScore <- updateMetric @DummyState "A" "a"

    pure newScore

runSomeFn :: Score
runSomeFn = State.evalDef @DummyState $ State.evalDefT @DummyState2 $ someFn

someFn :: MonadMetrics '[DummyState, DummyState2] m => m Score
someFn = do
    newScore <- updateMetric @DummyState "a" "a"

    pure newScore

someFn3 :: State.MonadStates '[DummyState, DummyState2] m => m Score
someFn3 = pure $ Score.Score 0

runSomeFn3 :: Score
runSomeFn3 = State.evalDef @DummyState $ State.evalDefT @DummyState2 $ someFn3

-- TODO [Ara] run/exec/eval

-- TODO [Ara] Destructure things





-------------------------------
-- === Utility Functions === --
-------------------------------

data DummyState = DummyState
    { _someNumber   :: !Int
    , _lastBuffer   :: ![Text]
    , _currentScore :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyState where
    def = DummyState def def def

instance NFData DummyState

instance Metric DummyState where
    updateMetric _ _ = do
        _ <- State.get @DummyState
        pure $ Score.Score 0

data DummyState2 = DummyState2
    { _someNumber1   :: !Int
    , _lastBuffer1   :: ![Text]
    , _currentScore1 :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyState2 where
    def = DummyState2 def def def

instance NFData DummyState2

instance Metric DummyState2 where
    updateMetric _ _ = do
        _ <- State.get @DummyState2
        pure $ Score.Score 0

