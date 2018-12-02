module New.Engine.MetricSpec where

import Prologue
import Test.Hspec

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Score       as Score
import qualified New.Engine.Metric           as Metric

import New.Engine.Data.Score (Score)
import New.Engine.Metric     (Metric)



---------------------------
-- === Testing Setup === --
---------------------------

data DummyMetric = DummyMetric
    { currentScore :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyMetric where
    def = DummyMetric $ Score.Score 1

instance NFData DummyMetric

instance Metric DummyMetric where
    updateMetric _ _ = State.get @DummyMetric >>= \st -> pure $ currentScore st

data DummyMetric2 = DummyMetric2
    { currentScore2 :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyMetric2 where
    def = DummyMetric2 $ Score.Score 1

instance NFData DummyMetric2

instance Metric DummyMetric2 where
    updateMetric _ _ = State.get @DummyMetric2 >>= \st ->
        pure $ currentScore2 st

data DummyMetric3 = DummyMetric3
    { currentScore3 :: !Score
    } deriving (Eq, Generic, Ord, Show)

instance Default DummyMetric3 where
    def = DummyMetric3 $ Score.Score 1

instance NFData DummyMetric3

instance Metric DummyMetric3 where
    updateMetric _ _ = State.get @DummyMetric3 >>= \st ->
        pure $ currentScore3 st

type MetricPasses = '[DummyMetric, DummyMetric2, DummyMetric3]

evalMetrics :: forall m a . MonadIO m => State.StatesT MetricPasses m a -> m a
evalMetrics metricFn = State.evalDefT @DummyMetric3
    . State.evalDefT @DummyMetric2
    . State.evalDefT @DummyMetric
    $ metricFn

combinedMetricUpdate :: forall m . Metric.MonadMetrics MetricPasses m => m Score
combinedMetricUpdate = Metric.updateMetrics @MetricPasses 'a' 'a' >>= pure

splitMetricUpdate :: forall m . Metric.MonadMetrics MetricPasses m => m Score
splitMetricUpdate = do
    res1 <- Metric.updateMetric @DummyMetric  'a' 'a'
    res2 <- Metric.updateMetric @DummyMetric2 'a' 'a'
    res3 <- Metric.updateMetric @DummyMetric3 'a' 'a'

    pure $ res1 + res2 + res3

expectedScore :: IO Score
expectedScore = pure $ Score.Score 3

gives :: IO Score -> IO Score -> Expectation
gives val1 val2 = do
    value1 <- val1
    value2 <- val2

    value1 `shouldBe` value2



-------------------
-- === Tests === --
-------------------

spec :: Spec
spec =
    describe "Updating Metrics" $ do
        it "is correct when evaluated as a group"
            $ (evalMetrics combinedMetricUpdate) `gives` expectedScore
        it "is correct when evaluated in isolation"
            $ (evalMetrics splitMetricUpdate) `gives` expectedScore
        it "is correct between evaluation strategies"
            $ (evalMetrics splitMetricUpdate) `gives`
                (evalMetrics combinedMetricUpdate)

