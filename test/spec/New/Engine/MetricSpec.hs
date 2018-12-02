module New.Engine.MetricSpec where

import Prologue
import Test.Hspec

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Match       as Match
import qualified New.Engine.Data.Score       as Score
import qualified New.Engine.Data.Substring   as Substring
import qualified New.Engine.Metric           as Metric

import New.Engine.Data.Score (Score)
import New.Engine.Metric     (Metric)



---------------------------
-- === Testing Setup === --
---------------------------

data DummyMetric = DummyMetric
    { _currentScore :: !Score
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DummyMetric

instance Default DummyMetric where
    def = DummyMetric $ Score.Score 0

instance NFData DummyMetric

instance Metric DummyMetric where
    updateMetric _ _ _ = State.get @DummyMetric >>= \st ->
        State.put @DummyMetric $ st & currentScore %~ (+1)

    getMetric = State.get @DummyMetric >>= \st ->
        pure $ st ^. currentScore

data DummyMetric2 = DummyMetric2
    { _currentScore2 :: !Score
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DummyMetric2

instance Default DummyMetric2 where
    def = DummyMetric2 $ Score.Score 0

instance NFData DummyMetric2

instance Metric DummyMetric2 where
    updateMetric _ _ _ = State.get @DummyMetric2 >>= \st ->
        State.put @DummyMetric2 $ st & currentScore2 %~ (+1)

    getMetric = State.get @DummyMetric2 >>= \st ->
        pure $ st ^. currentScore2

data DummyMetric3 = DummyMetric3
    { _currentScore3 :: !Score
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DummyMetric3

instance Default DummyMetric3 where
    def = DummyMetric3 $ Score.Score 0

instance NFData DummyMetric3

instance Metric DummyMetric3 where
    updateMetric _ _ _ = State.get @DummyMetric3 >>= \st ->
        State.put @DummyMetric3 $ st & currentScore3 %~ (+1)

    getMetric = State.get @DummyMetric3 >>= \st ->
        pure $ st ^. currentScore3

type MetricPasses = '[DummyMetric, DummyMetric2, DummyMetric3]

evalMetrics :: forall m a . MonadIO m => State.StatesT MetricPasses m a -> m a
evalMetrics metricFn = State.evalDefT @DummyMetric3
    . State.evalDefT @DummyMetric2
    . State.evalDefT @DummyMetric
    $ metricFn

mockedState :: Match.State
mockedState = Match.State mempty substring Substring.Equal 1 1 where
    substring = Substring.singleton $! Substring.fromPosition 0

combinedMetricUpdate :: forall m . Metric.MonadMetrics MetricPasses m => m ()
combinedMetricUpdate
    = Metric.updateMetrics @MetricPasses 'a' 'a' mockedState >> pure ()

splitMetricUpdate :: forall m . Metric.MonadMetrics MetricPasses m => m ()
splitMetricUpdate = Metric.updateMetric @DummyMetric  'a' 'a' mockedState
    >> Metric.updateMetric @DummyMetric2 'a' 'a' mockedState
    >> Metric.updateMetric @DummyMetric3 'a' 'a' mockedState
    >> pure ()

combinedUpdateAndGet :: forall m . Metric.MonadMetrics MetricPasses m => m Score
combinedUpdateAndGet = combinedMetricUpdate >> Metric.getMetrics @MetricPasses
    >>= \score -> pure score

splitUpdateAndGet :: forall m . Metric.MonadMetrics MetricPasses m => m Score
splitUpdateAndGet = do
    splitMetricUpdate

    res1 <- Metric.getMetric @DummyMetric
    res2 <- Metric.getMetric @DummyMetric2
    res3 <- Metric.getMetric @DummyMetric3

    pure $ res1 + res2 + res3

dualUpdateAndGet :: forall m . Metric.MonadMetrics MetricPasses m => m Score
dualUpdateAndGet = do
    Metric.updateMetrics @MetricPasses 'a' 'a' mockedState
    Metric.updateMetrics @MetricPasses 'a' 'a' mockedState

    Metric.getMetrics @MetricPasses >>= pure

expectedScore :: Score
expectedScore = Score.Score 3

gives :: IO Score -> IO Score -> Expectation
gives val1 val2 = do
    value1 <- val1
    value2 <- val2

    value1 `shouldBe` value2



-------------------
-- === Tests === --
-------------------

spec :: Spec
spec = do
    describe "Getting Metric Results" $ do
        it "is correct when evaluated as a group"
            $ (evalMetrics combinedUpdateAndGet) `gives` pure expectedScore
        it "is correct when evaluated in isolation"
            $ (evalMetrics splitUpdateAndGet) `gives` pure expectedScore
        it "is correct between evaluation strategies"
            $ (evalMetrics splitUpdateAndGet) `gives`
                (evalMetrics combinedUpdateAndGet)
    describe "Updating Metrics" $ do
        it "is correct when updated once"
            $ (evalMetrics combinedUpdateAndGet) `gives` pure expectedScore
        it "is correct when updated twice"
            $ (evalMetrics dualUpdateAndGet) `gives` pure (2 * expectedScore)

