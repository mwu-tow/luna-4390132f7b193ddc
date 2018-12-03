{-# LANGUAGE Strict #-}

module New.Engine.Metric.WordSuffixBonus where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Match       as Match
import qualified New.Engine.Data.Substring   as Substring

import Control.Lens              ((?~))
import Data.Char                 (isLetter, isLower, isUpper)
import New.Engine.Data.Score     (Score (Score))
import New.Engine.Data.Substring (Substring (Substring))
import New.Engine.Metric         (Metric (getMetric, updateMetric))



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

instance NFData  WordSuffixBonus

instance Metric  WordSuffixBonus where
    updateMetric dataChar matchKind updatedState = do
        bonusState <- State.get @WordSuffixBonus
        let posInData     = updatedState ^. Match.positionInData
            suffixes      = bonusState ^. wordsSuffixes
            revRange      = suffixes ^. Substring.reversedRange
            mayPrevChar   = bonusState ^. previousDataChar
            checkWordHead = \prev -> startsNewWord dataChar prev
            isWordHead    = maybe True checkWordHead mayPrevChar
            wordStart     = bonusState ^. lastWordStart
            updateWordStart
                = \prev -> if isWordHead then posInData - 1 else prev
            addPosition  = let pos = posInData - 1 in
                Substring.addPosition pos suffixes
            deleteLastSuffix []                 = []
            deleteLastSuffix prev@((!h) : (!t)) = let
                hBeg   = h ^. Substring.begin
                hEnd   = h ^. Substring.end
                newLen = wordStart - hBeg
                newH   = h & Substring.len .~ newLen
                in if hEnd < wordStart then prev
                    else if newLen > 0 then newH : t
                    else t
            updatedWordsSuffixes = if matchKind == Match.Equal then addPosition
                else if isWordHead then suffixes
                else Substring $! deleteLastSuffix revRange
        State.modify_ @WordSuffixBonus $! \s -> s
            & previousDataChar ?~ dataChar
            & lastWordStart    %~ updateWordStart
            & wordsSuffixes    .~ updatedWordsSuffixes
    getMetric _ = do
        mult     <- State.use @WordSuffixBonus multiplier
        suffixes <- State.use @WordSuffixBonus wordsSuffixes
        let revRange        = suffixes ^. Substring.reversedRange
            accRangeLength  = \r -> let
                rLen = r ^. Substring.len
                in rLen * (rLen + 1) `quot` 2
            appendAccLength = \acc r -> acc + accRangeLength r
            points          = foldl appendAccLength def revRange
        pure $! Score $! mult * points

