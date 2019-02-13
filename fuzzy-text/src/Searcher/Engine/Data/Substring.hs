{-# LANGUAGE Strict #-}

module Searcher.Engine.Data.Substring where

import Prologue hiding (Index)

import Control.Lens (Getter, makePrisms, to)



-------------------
-- === Range === --
-------------------

-- === Definition === --

data Range = Range
    { _begin :: Int
    , _len   :: Int
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
-- === Substring === --
-----------------------

-- TODO [Ara] Swap the `[Range]` for `Data.Sequence Range` for better perf.

-- === Definition === --

newtype Substring = Substring
    { _reversedRange :: [Range]
    } deriving (Eq, Generic, Show)

makeLenses ''Substring


-- === API === --

range :: Getter Substring [Range]
range = reversedRange . to reverse
{-# INLINE range #-}

totalLength :: Getter Substring Int
totalLength = to $! \s -> let
    addLengths = \acc r -> acc + r ^. len
    revRange   = s ^. reversedRange
    in foldl addLengths def revRange
{-# INLINE totalLength #-}

singleton :: Range -> Substring
singleton r = Substring [r]
{-# INLINE singleton #-}

addPosition :: Int -> Substring -> Substring
addPosition pos m = addRange (fromPosition pos) m
{-# INLINE addPosition #-}

addRange :: Range -> Substring -> Substring
addRange r m = merge (singleton r) m
{-# INLINE addRange #-}

merge :: Substring -> Substring -> Substring
merge m1 m2 = Substring $ mergeRanges range1 range2 where
    range1 = m1 ^. reversedRange
    range2 = m2 ^. reversedRange
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
        mergedH   = Range minBeg mergedLen
        in if h1Beg > h2Beg
                then mergeRanges prev2 prev1
            else if h2Beg > h1End
                then let newT1 = mergeRanges prev1 t2 in h2 : newT1
            else mergeRanges (mergedH : t1) t2
{-# INLINE merge #-}


-- === Instances === --

instance Mempty    Substring where mempty = Substring mempty
instance Default   Substring where def    = mempty
instance NFData    Substring
instance Semigroup Substring where (<>) = merge



------------------
-- === Kind === --
------------------

-- === Definition === --

data Kind
    = Other
    | FullMatch
    | CaseInsensitiveEqual
    | Equal
    deriving (Eq, Generic, Ord, Show)
makePrisms ''Kind


-- === Instances === --

instance NFData Kind

