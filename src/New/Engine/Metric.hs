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

-- TODO [Ara] Brief module documentation (to help explain the arcane).
-- TODO [Ara] eval
-- TODO [Ara] Test suite

-- `ss` is the list of state types, of which st is a member
-- Takes a computation in soome number of metrics
evalT :: (Metrics ss, P.Monad m) => ()
evalT = undefined

-- Needs to build the chain of State.evalT @s ...
-- This (as one function) then gets called on the computation.
class P.Monad m => Eval (t :: k) m where
    evalT_ :: forall s1 s2 n a . (P.Monad n, Default s1, Default s2, n ~ State.StateT s1 m)
        => State.StateT s2 n a -> State.StateT s1 m a

instance (Monad s m, Eval ss m) => Eval ((s ': ss) :: [Type]) m where
    evalT_ = undefined

instance P.Monad m => Eval ('[] :: [Type]) m where
    evalT_ = undefined

instance Monad s m => Eval (s :: Type) m where
    evalT_ = State.evalDefT

---------------------------------------- Sep into API and instances
class P.Monad m => Update (t :: k) m where
    updateMetrics :: Text -> Text -> m Score

instance (Monad s m, Update ss m) => Update ((s ': ss) :: [Type]) m where
    updateMetrics tx1 tx2 = do
        res1 <- updateMetrics @s  tx1 tx2
        res2 <- updateMetrics @ss tx1 tx2

        pure $ res1 + res2

instance P.Monad m => Update ('[] :: [Type]) m where
    updateMetrics _ _ = pure $ def @Score

instance Monad s m => Update (s :: Type) m where
    updateMetrics tx1 tx2 = updateMetric @s tx1 tx2



--------------------------
-- === Testing Code === --
--------------------------

type MetricPasses = '[DummyState, DummyState2, DummyState3]

-- someFn :: State.StateT S1 (State.StateT S2 (State.StateT S3 n)) a
--                           ^                                   ^
--                        st |_______________ m _________________| a

runSomeFn :: IO Score
-- runSomeFn = evalT @MetricPasses someFn
runSomeFn = (State.evalDefT @DummyState
    . State.evalDefT @DummyState3
    . State.evalDefT @DummyState2) someFn

someFn :: forall m . MonadMetrics MetricPasses m => m Score
someFn = do
    newScore <- updateMetrics @MetricPasses "a" "a"

    pure newScore

data DummyState = DummyState
    { _currentScore :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyState where
    def = DummyState def

instance NFData DummyState

instance Metric DummyState where
    updateMetric _ _ = do
        _ <- State.get @DummyState
        pure $ Score.Score 1

data DummyState2 = DummyState2
    { _currentScore2 :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyState2 where
    def = DummyState2 def

instance NFData DummyState2

instance Metric DummyState2 where
    updateMetric _ _ = do
        _ <- State.get @DummyState2
        pure $ Score.Score 1

data DummyState3 = DummyState3
    { _currentScore3 :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyState3 where
    def = DummyState3 def

instance NFData DummyState3

instance Metric DummyState3 where
    updateMetric _ _ = do
        _ <- State.get @DummyState3
        pure $ Score.Score 1

