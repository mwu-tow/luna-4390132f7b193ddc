{-# LANGUAGE Strict    #-}
{-# LANGUAGE PolyKinds #-}

module New.Engine.Metric where

import Prologue hiding (Monad)

import Control.Monad.State.Layered as State
import New.Engine.Data.Score       as Score

-- TODO [Ara] Design for metrics
-- Idea for metric as a typeclass, with type-applied arguments.
-- Can be compile-time only.
-- Needs next letter in query and next letter in database.
-- Return current score. (as Int)
-- Can potentially be stateful.
-- e.g. Prefix matcher contains state about collected points, multipliers and
-- context info.

data DummyState = DummyState
    { _someNumber   :: !Int
    , _lastBuffer   :: ![Text]
    , _currentScore :: !Score
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DummyState

instance Default DummyState where
    def = DummyState def def def

instance NFData DummyState

data DummyState2 = DummyState2
    { _someNumber1   :: !Int
    , _lastBuffer1   :: ![Text]
    , _currentScore1 :: !Score
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DummyState2

instance Default DummyState2 where
    def = DummyState2 def def def

instance NFData DummyState2


--------------------
-- === Metric === --
--------------------

-- === Definition === --

-- Stack of States, each of which is restricted to the MetricState typeclass.
-- A function to `runMetrics` and to `updateMetrics` that operate over the
-- type-level list.

-- Alternatively, just a typeclass, extended by the state type.

-- === API === --


-- === Instances === --

someFn :: State.MonadStates '[DummyState, DummyState2] m => m Score
someFn = do
    st  <- get @DummyState
    st2 <- get @DummyState2

    newScore <- updateMetric @DummyState "a" "a"

    pure newScore

-- Minimal is updateMetric as the rest are defaulted.

updateMetric :: forall s m . State.Monad s m => Text -> Text -> m Score
updateMetric _ _ = pure $ Score 1

runMetric :: forall s m a . State.Monad s m => m a -> (s, a)
runMetric = undefined

evalMetric :: forall s m a . State.Monad s m => m a -> a
evalMetric = undefined

execMetric :: forall s m a . State.Monad s m => m a -> s
execMetric = undefined


-------------------------------
-- === Utility Functions === --
-------------------------------

-- class (Default a, NFData a) => MonadMetric a m where
    -- updateMetric :: Text -> Text -> m a
    -- runMetric    :: m a -> Score

-- type family MonadMetrics as m :: Constraint where
    -- MonadMetrics (a ': as) m = (MonadMetric a m, MonadMetrics as m)
    -- MonadMetrics '[]       m = ()

-- TODO [Ara] Class for the updateMetrics function.



-------------------------------------------------------------------------------

