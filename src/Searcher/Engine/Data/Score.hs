module Searcher.Engine.Data.Score where

import Searcher.Engine.Prelude


type Weight = Double
type Fixed  = Int

data Score = Score
    { _fixed  :: Fixed
    , _points :: Int
    , _weight :: Weight
    } deriving (Eq, Generic, Show)

makeLenses ''Score

instance Default Score where
    def = Score def def def

total :: Getter Score Double
total = to $ \s ->
    fromIntegral (s ^. fixed) + fromIntegral (s ^. points) * s ^. weight


data Scoring = Scoring
    { _mismatchPenalty :: Int
    , _skipPenalty     :: Int
    , _prefixBonus     :: Int
    , _sequenceBonus   :: Int
    , _suffixBonus     :: Int
    , _wordPrefixBonus :: Int
    , _wordSuffixBonus :: Int
    } deriving (Eq, Show)

makeLenses ''Scoring

instance Default Scoring where
    def = Scoring
        (-4) -- mismatchPenalty
        (-4) -- skipPenalty
        12   -- prefixBonus
        10   -- sequenceBonus
        4    -- suffixBonus
        6    -- wordPrefixBonus
        3    -- wordSuffixBonus

