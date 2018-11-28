{-# LANGUAGE Strict    #-}
{-# LANGUAGE PolyKinds #-}

module New.Engine.Metric where

import Prologue

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



--------------------
-- === Metric === --
--------------------

-- === Definition === --

class (Default a, NFData a) => Metric a where
    updateMetric :: Text -> Text -> State.State a Score
    runMetric    :: State.State a Score -> Score

-- TODO [Ara] Factor out metric state as associated type family.
-- TODO [Ara] Possible to have a `runMetrics` function over a type-level list?
-- TODO [Ara] Similarly with `updateMetrics` that returns the current compound
-- score.


-- === API === --


-- === Instances === --



-------------------------
-- === Test Metric === --
-------------------------


-------------------------------
-- === Utility Functions === --
-------------------------------

type Test = '[DummyState]

type Test2 a = State.MonadStates Test a

data DummyState = DummyState
    { _someNumber   :: !Int
    , _lastBuffer   :: ![Text]
    , _currentScore :: !Score
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DummyState

instance Default DummyState where
    def = DummyState def def def

instance NFData DummyState

instance Metric DummyState where
    updateMetric = undefined

-- TODO [Ara] Now the question is how to make this usable.

testMetrics :: IO ()
testMetrics = print ("Foo" :: String)

