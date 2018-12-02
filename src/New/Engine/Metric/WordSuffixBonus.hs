{-# LANGUAGE Strict #-}

module New.Engine.Metric.WordSuffixBonus where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified New.Engine.Data.Match     as Match
import qualified New.Engine.Data.Substring as Substring

import Control.Lens ((?~))
import New.Engine.Metric (Metric (updateMetric, getMetric))
import New.Engine.Data.Score (Score (Score))
import New.Engine.Data.Substring (Substring (Substring))
import Data.Char (isLower, isUpper, isLetter)



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

instance Default WordSuffixBonus where def = WordSuffixBonus 6 def def def

instance NFData  WordSuffixBonus

instance Metric  WordSuffixBonus where
    updateMetric c1 c2 updatedState = undefined -- do
        -- bonusState <- State.get @WordSuffixBonus
        -- let posInData     = updatedState ^. Match.positionInData
            -- suffixes      = bonusState ^. wordsSuffixes
            -- revRange      = suffixes ^. Substring.reversedRange
            -- mayPrevChar   = bonusState ^. previousDataChar
            -- isC2WordHead  = \prev -> startsNewWord c2 prev
            -- isWordHead    = maybe True isC2WordHead mayPrevChar
            -- wordStart     = bonusState ^. lastWordStart
            -- updateWordStart
                -- = \prev -> if isWordHead then posInData - 1 else prev
            -- addPosition  = let pos = posInData - 1 in
                -- Substring.addPosition pos suffixes
            -- deleteLastSuffix []            = []
            -- deleteLastSuffix ((!h) : (!t)) = let
                -- hBeg   = h ^. Substring.begin
                -- newLen = wordStart - hBeg
                -- newH   = h & Substring.len .~ newLen
                -- in if newLen > 0 then newH : t else t
            -- updatedWordsSuffixes = if c1 == c2 then addPosition
                -- else if isWordHead then suffixes
                -- else Substring $! deleteLastSuffix revRange
        -- State.modify_ @WordSuffixBonus $! \s -> s
            -- & previousDataChar ?~ c2
            -- & lastWordStart    %~ updateWordStart
            -- & wordsSuffixes    .~ updatedWordsSuffixes
        -- mult <- State.use @WordSuffixBonus multiplier
        -- let newRevRange     = updatedWordsSuffixes ^. Substring.reversedRange
            -- accRangeLength  = \r -> let
                -- rLen = r ^. Substring.len
                -- in rLen * (rLen + 1) `quot` 2
            -- appendAccLength = \acc r -> acc + accRangeLength r
            -- points          = foldl appendAccLength def newRevRange
        -- pure $! Score $! mult * points
    getMetric = undefined

