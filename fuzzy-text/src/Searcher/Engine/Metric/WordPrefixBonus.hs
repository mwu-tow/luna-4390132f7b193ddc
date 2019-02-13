{-# LANGUAGE Strict #-}

module Searcher.Engine.Metric.WordPrefixBonus where

import Prologue

import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Substring as Substring
import qualified Searcher.Engine.Metric         as Metric

import Control.Lens                   ((?~))
import Data.Char                      (isLetter, isLower, isUpper)
import Searcher.Engine.Data.Score     (Score (Score))
import Searcher.Engine.Data.Substring (Substring)



-----------------------------
-- === WordPrefixBonus === --
-----------------------------

-- === Definition === --

data WordPrefixBonus = WordPrefixBonus
    { _multiplier       :: Int
    , _wordsPrefixes    :: Substring
    , _previousDataChar :: Maybe Char
    } deriving (Generic, Show)
makeLenses ''WordPrefixBonus


-- === API === --

startsNewWord :: Char -> Char -> Bool
startsNewWord c prevC = let xor a b = (a && not b) || (not a && b)
    in (isLetter prevC `xor` isLetter c) || (isLower prevC && isUpper c)
{-# INLINE startsNewWord #-}


-- === Instances === --

instance Default WordPrefixBonus where
    def = WordPrefixBonus 6 def def
    {-# INLINE def #-}

instance NFData WordPrefixBonus

instance Metric.State WordPrefixBonus where
    updateMetric metricSt dataChar charMatch updatedState = let
        prefixes     = metricSt ^. wordsPrefixes
        mayPrevChar  = metricSt ^. previousDataChar
        posInData    = updatedState ^. Match.positionInData
        revRange     = prefixes ^. Substring.reversedRange
        mayLastRange = head revRange
        mayRangeEnd  = view Substring.end <$> mayLastRange
        appendChar   = \end -> if posInData - 1 == end
            then Substring.addPosition end prefixes
            else prefixes
        appendWordHead = Substring.addPosition (posInData - 1) prefixes
        isWordHead = case mayPrevChar of
            Nothing -> True
            Just prevC -> startsNewWord dataChar prevC
        updatedPrefixes = if charMatch /= Match.NotMatched
            then if isWordHead
                then appendWordHead
                else maybe prefixes appendChar mayRangeEnd
            else prefixes
        in metricSt
            & wordsPrefixes .~ updatedPrefixes
            & previousDataChar ?~ dataChar
    {-# INLINE updateMetric #-}

    getMetric metricSt _ = let
        revRange        = metricSt ^. wordsPrefixes ^. Substring.reversedRange
        accRangeLength  = \r -> let rlen = r ^. Substring.len in
            rlen * (rlen + 1) `quot` 2
        appendAccLength = \acc r -> acc + accRangeLength r
        points = foldl' appendAccLength def revRange
        in Score $! (metricSt ^. multiplier) * points
    {-# INLINE getMetric #-}

