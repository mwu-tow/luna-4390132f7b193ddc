{-# LANGUAGE Strict    #-}
{-# LANGUAGE PolyKinds #-}

module New.Engine.Metric where

import Prologue hiding (Monad)

import qualified Prologue as P (Monad)

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Score       as Score

import New.Engine.Data.Score (Score)




--------------------
-- === Metric === --
--------------------

-- === Definition === --

class (Default s, NFData s) => Metric s where
    updateMetric :: State.Monad (s :: Type) m => Text -> Text -> m Score

type Monad s m        = (P.Monad m, Metric s, State.Monad s m)
type MonadMetrics s m = (P.Monad m, Metrics s, State.MonadStates s m)

type family Metrics ss :: Constraint where
    Metrics (s ': ss) = (Metric s, Metrics ss)
    Metrics '[]       = ()


-- === API === --

-- TODO [Ara] run/exec/eval
-- TODO [Ara] updateMetrics

class P.Monad m => Update (t :: k) m where
    updateMetrics :: State.Monad t m => Text -> Text -> m Score

instance Monad s m => Update (s :: Type) m where
    updateMetrics tx1 tx2 = updateMetric @s tx1 tx2

instance Monad s m => Update ((s ': '[]) :: [Type]) m where
    updateMetrics tx1 tx2 = updateMetric @s tx1 tx2

instance (Monad s m, Update ss m) => Update ((s ': ss) :: [Type]) m where
    updateMetrics tx1 tx2 = do
        res1 <- updateMetrics @s  tx1 tx2
        res2 <- updateMetrics @ss tx1 tx2

        pure $ res1 + res2

















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

