{-# LANGUAGE Strict #-}

module Searcher.Engine.Metric.WordSuffixBonus where

import Prologue

import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Substring as Substring

import Control.Lens                   ((?~))
import Data.Char                      (isLetter, isLower, isUpper)
import Searcher.Engine.Data.Score     (Score (Score))
import Searcher.Engine.Data.Substring (Substring (Substring))
import Searcher.Engine.Metric         (Metric (getMetric, updateMetric))



-----------------------------
-- === WordSuffixBonus === --
-----------------------------

-- === Definition === --

data WordSuffixBonus = WordSuffixBonus
    { _multiplier       :: Int
    , _wordsSuffixes    :: Substring
    , _previousDataChar :: Maybe Char
    , _lastWordStart    :: Int
    } deriving (Generic, Show)
makeLenses ''WordSuffixBonus


-- === API === --

startsNewWord :: Char -> Char -> Bool
startsNewWord c prevC = let xor a b = (a && not b) || (not a && b)
    in (isLetter prevC `xor` isLetter c) || (isLower prevC && isUpper c)


-- === Instances === --

instance Default WordSuffixBonus where def = WordSuffixBonus 3 def def def

instance NFData WordSuffixBonus

instance Metric WordSuffixBonus where
    updateMetric metricSt dataChar matchKind updatedState = let
        posInData       = updatedState ^. Match.positionInData
        suffixes        = metricSt ^. wordsSuffixes
        revRange        = suffixes ^. Substring.reversedRange
        mayPrevChar     = metricSt ^. previousDataChar
        checkWordHead   = \prev -> startsNewWord dataChar prev
        isWordHead      = maybe True checkWordHead mayPrevChar
        wordStart       = metricSt ^. lastWordStart
        updateWordStart = \prev ->
            if isWordHead then posInData - 1 else prev
        addPosition = let pos = posInData - 1 in
            Substring.addPosition pos suffixes
        deleteLastSuffix [] = []
        deleteLastSuffix prev@((!h) : (!t)) = let
            hBeg   = h ^. Substring.begin
            hEnd   = h ^. Substring.end
            newLen = wordStart - hBeg
            newH   = h & Substring.len .~ newLen
            in if hEnd < wordStart then prev
                else if newLen > 0 then newH : t
                else t
        updatedWordsSuffixes = if matchKind == Match.Equal
            then addPosition
            else if isWordHead then suffixes
            else Substring $! deleteLastSuffix revRange
        in metricSt
            & previousDataChar ?~ dataChar
            & lastWordStart    %~ updateWordStart
            & wordsSuffixes    .~ updatedWordsSuffixes

    getMetric metricSt _ = let
        revRange        = metricSt ^. wordsSuffixes ^. Substring.reversedRange
        accRangeLength  = \r -> let rlen = r ^. Substring.len in
            rlen * (rlen + 1) `quot` 2
        appendAccLength = \acc r -> acc + accRangeLength r
        points = foldl' appendAccLength def revRange
        in Score $! (metricSt ^. multiplier) * points

