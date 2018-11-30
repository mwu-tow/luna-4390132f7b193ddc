
-------------------------------------------------------------------------------
-- |
-- Metric Class
--
-- This module provides functionality for the creation of your own metrics for
-- use in the fuzzy-text searching algorithm.
--
-- A metric is a State type and accompanying stateful function `updateMetric` to
-- be implemented for that type. Your state type should have an implementation
-- of the `Metric` typeclass, and then some type-level resolution takes place to
-- ensure that it is updated internally as appropriate. You need only pass in a
-- list of the types of the metrics you would like computed.
--
-- The essence of a metric is that it computes a score based on the input text
-- and the text at the current node in the Trie.
--
-- This may not be clear, at first, so let's work through an example as follows.
-- We're going to create a metric `DummyMetric` that actually doesn't do
-- anything but return its default score at each call. You will need to import
-- `Control.Monad.State.Layered` to work with the state representation.
--
-- 1. Create your metric's state type:
--
--      ```
--      data DummyMetric = DummyMetric
--          { currentScore :: !Score
--          } deriving (Eq, Ord, Show)
--      ```
--
-- 2. Implement `Metric` for your type:
--
--      ```
--      instance Metric DummyMetric where
--          updateMetric _ _ = State.get @DummyMetric >>= \st ->
--              pure $ currentScore st
--      ```
--
-- 3. You'll note that Metric requires you to implement `Default` and `NFData`,
--    so let's do that:
--
--      ```
--      instance Default DummyMetric where
--          def = DummyMetric def
--
--      instance NFData DummyMetric
--      ```
--
-- 4. Now you have a metric, you can make a few more. Let's assume we also have
--    `DummyMetric2` and `DummyMetric3`, so we can make a type-level list of
--    these metrics. You'll need to have `-XDataKinds` enabled for this to work.
--
--      ```
--      type MetricPasses = '[DummyMetric, DummyMetric2, DummyMetric3]
--      ```
--
-- 5. Now unfortunately, getting the metrics to execute properly is a bit of a
--    slog, but we're working on improving it using that type-level list. The
--    library's search function is parametrised over your types, so you need to
--    do the following:
--
--      ```
--      runSearch :: (Monad m) => m (Map Index Result)
--      runSearch = State.evalDefT @DummyMetric
--          . State.evalDefT @DummyMetric2
--          . State.evalDefT @DummyMetric3
--          $ search @MetricPasses inputText textTree
--      ```
--
-- This will then 'just work'. As the search is run, the update functions on
-- all of your metrics will be called, and used to score the results returned by
-- the search.
--
-- For examples of actually useful metrics, and further guidance on how to work
-- with this interface, please see the metrics included with the library in the
-- `Engine.Metric.*` modules.

-- TODO [Ara] Test suite

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

-- TODO [Ara] eval

-- `ss` is the list of state types, of which st is a member
-- Takes a computation in soome number of metrics
evalT :: (Metrics ss, P.Monad m) => ()
evalT = undefined

-- Needs to build the chain of State.evalT @s ...
-- This (as one function) then gets called on the computation.
class (P.Monad m, P.Monad n, Default s1, Default s2)
    => Eval (t :: k) m n s1 s2 where
    evalT_ :: forall a . State.StateT s2 n a -> State.StateT s1 m a

-- instance (P.Monad m, Eval ss m n s1 s2, Default s1, Default s2, n ~ State.StateT s1 m)
    -- => Eval ((s ': ss) :: [Type]) m n s1 s2 where
    -- evalT_ layer =
--
-- instance (P.Monad m, P.Monad n, Default s1, Default s2, s1 ~ s2, n ~ m)
    -- => Eval ('[] :: [Type]) m n s1 s2 where
    -- evalT_ layer = layer

instance (P.Monad m, P.Monad n, Default s1, Default s2, n ~ State.StateT s1 m)
    => Eval (s :: Type) m n s1 s2 where
    evalT_ layer = State.evalDefT @s2 layer

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

-- someFn :: State.StateT S1 (State.StateT S2 (State.StateT S3 n)) a
--                           ^                                   ^
--                        st |_______________ m _________________| a

type MetricPasses = '[DummyMetric, DummyMetric2, DummyMetric3]

runSomeFn :: IO Score
-- runSomeFn = evalT @MetricPasses someFn
runSomeFn = (State.evalDefT @DummyMetric
    . State.evalDefT @DummyMetric3
    . State.evalDefT @DummyMetric2) someFn

someFn :: forall m . MonadMetrics MetricPasses m => m Score
someFn = do
    newScore <- updateMetrics @MetricPasses "a" "a"

    pure newScore

data DummyMetric = DummyMetric
    { currentScore :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyMetric where
    def = DummyMetric def

instance NFData DummyMetric

instance Metric DummyMetric where
    updateMetric _ _ = State.get @DummyMetric >>= \st -> pure $ currentScore st

data DummyMetric2 = DummyMetric2
    { _currentScore2 :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyMetric2 where
    def = DummyMetric2 def

instance NFData DummyMetric2

instance Metric DummyMetric2 where
    updateMetric _ _ = do
        _ <- State.get @DummyMetric2
        pure $ Score.Score 1

data DummyMetric3 = DummyMetric3
    { _currentScore3 :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyMetric3 where
    def = DummyMetric3 def

instance NFData DummyMetric3

instance Metric DummyMetric3 where
    updateMetric _ _ = do
        _ <- State.get @DummyMetric3
        pure $ Score.Score 1

