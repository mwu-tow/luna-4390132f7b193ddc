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
    } deriving (Eq, Ord, Show)

makeLenses ''Range

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

newtype Match
    = Match { _reversed_range :: [Range] } deriving (Eq, Show)

makeLenses ''Match

instance Mempty    Match where mempty = Match mempty
instance Semigroup Match where (<>)   = merge

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
merge m1 m2 = do
    let mergeRanges r1 [] = r1
        mergeRanges [] r2 = r2
        mergeRanges prev1@(h1:t1) prev2@(h2:t2)
            | h1 ^. begin > h2 ^. begin = mergeRanges prev2 prev1
            | h1 ^. begin > h2 ^. end   = h1 : mergeRanges t1 prev2
            | otherwise                 = mergeRanges (newH1:t1) t2 where
                minBeg = min (h1 ^. begin) (h2 ^. begin)
                maxEnd = max (h1 ^. end)   (h2 ^. end)
                newH1  = Range minBeg $ maxEnd - minBeg
    Match $ mergeRanges (m1 ^. reversed_range) (m2 ^. reversed_range)
{-# INLINE merge #-}


-----------------------
-- === MatchKind === --
-----------------------

-- === Definition === --

data MatchKind
    = CaseSensitiveEquality
    | CaseInsensitiveEquality
    | AllCharsMatched
    | NotFullyMatched
    deriving (Eq, Ord, Show)

makePrisms ''MatchKind


