module Text.ScopeSearcher.Score where

import           Control.Lens
import           Data.Default

type Score = Double

data ScoreInfo = ScoreInfo { _value      :: Double
                           , _origValue  :: Double
                           , _n          :: Double
                           , _m          :: Double
                           , _bounty     :: Double
                           , _bounty1    :: Double
                           , _bounty2    :: Double
                           , _bounty3    :: Double
                           , _denom      :: Double
                           , _substrings :: Double
                           , _prefixSize :: Double
                           , _subtract   :: Double
                           , _capTouched :: Double
                           , _totalCap   :: Double
                           } deriving (Show, Eq)

makeLenses ''ScoreInfo

instance Ord ScoreInfo where
    scoreA `compare` scoreB = (scoreA ^. value) `compare` (scoreB ^. value)

instance Default ScoreInfo where
    def = ScoreInfo def def def def def def def def def def def def def def

scale :: Double -> ScoreInfo -> ScoreInfo
scale factor score = score & value %~ (* factor)
