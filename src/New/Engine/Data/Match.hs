{-# LANGUAGE Strict #-}
module New.Engine.Data.Match where

import Prologue hiding (Index)

import Control.Lens (Getter, makePrisms, to)



-------------------
-- === Range === --
-------------------


-- === Definition === --

data Range = Range
    { _begin :: !Int
    , _len   :: !Int
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Range

instance NFData Range


-- === API === --

end :: Getter Range Int
end = to $ \r -> r ^. begin + r ^. len
{-# INLINE end #-}

fromPosition :: Int -> Range
fromPosition pos = Range pos 1
{-# INLINE fromPosition #-}



-----------------------
-- === MatchKind === --
-----------------------


-- === Definition === --

newtype Match = Match 
    { _reversed_range :: [Range]
    } deriving (Eq, Generic, Show)

makeLenses ''Match

instance Mempty    Match where mempty = Match mempty
instance NFData    Match
instance Semigroup Match where (<>) = merge


-- === API === --

range :: Getter Match [Range]
range = reversed_range . to reverse
{-# INLINE range #-}

singleton :: Range -> Match
singleton r = Match [r]
{-# INLINE singleton #-}

addPosition :: Int -> Match -> Match
addPosition pos m = addRange (fromPosition pos) m
{-# INLINE addPosition #-}

addRange :: Range -> Match -> Match
addRange r m = merge (singleton r) m
{-# INLINE addRange #-}

merge :: Match -> Match -> Match
merge m1 m2 = Match $ mergeRanges range1 range2 where
    range1 = m1 ^. reversed_range 
    range2 = m2 ^. reversed_range 
    mergeRanges r1 [] = r1
    mergeRanges [] r2 = r2
    mergeRanges prev1@((!h1) : (!t1)) prev2@((!h2) : (!t2)) = let
        h1Beg     = h1 ^. begin
        h2Beg     = h2 ^. begin
        h1End     = h1 ^. end
        h2End     = h2 ^. end
        minBeg    = min h1Beg h2Beg
        maxEnd    = max h1End h2End
        mergedLen = maxEnd - minBeg
        mergedH1  = Range minBeg mergedLen
        new1      = mergedH1:t1
        in if h1Beg > h2Beg 
                then mergeRanges prev2 prev1
            else if h1Beg > h2End 
                then let newT1 = mergeRanges t1 prev2 in h1 : newT1
            else mergeRanges new1 t2
{-# INLINE merge #-}



-----------------------
-- === MatchKind === --
-----------------------


-- === Definition === --

data MatchKind
    = NotFullyMatched
    | AllCharsMatched
    | CaseInsensitiveEquality
    | CaseSensitiveEquality
    deriving (Eq, Generic, Ord, Show)
makePrisms ''MatchKind

instance NFData MatchKind


