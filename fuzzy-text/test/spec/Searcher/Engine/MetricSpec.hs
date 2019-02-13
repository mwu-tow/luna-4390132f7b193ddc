module Searcher.Engine.MetricSpec where

import Prologue
import Test.Hspec

import qualified Data.TypeMap.Strict            as TypeMap
import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Score     as Score
import qualified Searcher.Engine.Data.Substring as Substring
import qualified Searcher.Engine.Metric         as Metric

import Searcher.Engine.Data.Score (Score)
import Searcher.Engine.Metric     (Metric)



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

instance Metric.State DummyMetric where
    updateMetric st _ _ _ = st & currentScore %~ (+1)

    getMetric st _ = st ^. currentScore

data DummyMetric2 = DummyMetric2
    { _currentScore2 :: !Score
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DummyMetric2

instance Default DummyMetric2 where
    def = DummyMetric2 $ Score.Score 0

instance NFData DummyMetric2

instance Metric.State DummyMetric2 where
    updateMetric st _ _ _ = st & currentScore2 %~ (+1)

    getMetric st _ = st ^. currentScore2

data DummyMetric3 = DummyMetric3
    { _currentScore3 :: !Score
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DummyMetric3

instance Default DummyMetric3 where
    def = DummyMetric3 $ Score.Score 0

instance NFData DummyMetric3

instance Metric.State DummyMetric3 where
    updateMetric st _ _ _ = st & currentScore3 %~ (+1)

    getMetric st _ = st ^. currentScore3

mockedMatchState :: Match.State
mockedMatchState = Match.State mempty substring Substring.Equal 1 1 where
    substring = Substring.singleton $! Substring.fromPosition 0

expectedScore :: Score
expectedScore = Score.Score 3

type MyStates = '[DummyMetric, DummyMetric2, DummyMetric3]

combinedMetricUpdate :: Metric MyStates -> Metric MyStates
combinedMetricUpdate ms = Metric.update ms 'a' Match.Equal mockedMatchState

splitMetricUpdate :: Metric MyStates -> Metric MyStates
splitMetricUpdate ms = let
    st1 = Metric.updateMetric (TypeMap.getElem @DummyMetric ms) 'a' Match.Equal
        mockedMatchState
    st2 = Metric.updateMetric (TypeMap.getElem @DummyMetric2 ms) 'a' Match.Equal
        mockedMatchState
    st3 = Metric.updateMetric (TypeMap.getElem @DummyMetric3 ms) 'a' Match.Equal
        mockedMatchState
    in TypeMap.setElem st3 . TypeMap.setElem st2 $ TypeMap.setElem st1 ms

combinedUpdateAndGet :: Score
combinedUpdateAndGet =
    Metric.get (combinedMetricUpdate $ Metric.make @MyStates) mockedMatchState

splitUpdateAndGet :: Score
splitUpdateAndGet =
    Metric.get (splitMetricUpdate $ Metric.make @MyStates) mockedMatchState

dualUpdateAndGet :: Score
dualUpdateAndGet = Metric.get res mockedMatchState where
    res = combinedMetricUpdate . combinedMetricUpdate $ Metric.make @MyStates



-------------------
-- === Tests === --
-------------------

spec :: Spec
spec = do
    describe "Getting Metric Results" $ do
        it "is correct when evaluated as a group"
            $ combinedUpdateAndGet `shouldBe` expectedScore
        it "is correct when evaluated in isolation"
            $ splitUpdateAndGet `shouldBe` expectedScore
        it "is correct between evaluation strategies"
            $ splitUpdateAndGet `shouldBe` combinedUpdateAndGet

    describe "Updating Metrics" $ do
        it "is correct when updated once"
            $ combinedUpdateAndGet `shouldBe` expectedScore
        it "is correct when updated twice"
            $ dualUpdateAndGet `shouldBe` (2 * expectedScore)

