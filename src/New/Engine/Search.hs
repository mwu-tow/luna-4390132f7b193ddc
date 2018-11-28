module New.Engine.Search where

import Prologue hiding (Index)

import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Index       as Index
import qualified New.Engine.Data.Match       as Match
import qualified New.Engine.Data.Tree        as Tree

import Data.Char             (isLetter, isUpper, toLower, toUpper)
import Data.Map              (Map)
import New.Engine.Data.Index (Index)
import New.Engine.Data.Match (Match, MatchKind (CaseInsensitiveEquality, CaseSensitiveEquality, AllCharsMatched, NotFullyMatched))


--------------------
-- === Result === --
--------------------

-- === Definition === --

data Result = Result
    { _kind   :: !MatchKind
    , _match  :: !Match
    , _points :: {-# UNPACK #-} !Int
    } deriving (Eq, Generic, Show)
makeLenses ''Result

instance NFData Result
instance Ord    Result where
    -- TODO[LJK]: This should be replaced with scoring match kind as soon as old algorithm is recreated
    compare r1 r2 = compareResults where
        matchTypeOrd = (r1 ^. kind)   `compare` (r2 ^. kind)
        pointsOrd    = (r1 ^. points) `compare` (r2 ^. points)
        compareResults
            | matchTypeOrd /= EQ = matchTypeOrd
            | otherwise          = pointsOrd

-- === API === --

search :: Text -> Tree.Node -> Map Index Result
search query tree
    = recursiveSearch query tree CaseSensitiveEquality mempty 0 mempty

-- TODO[LJK]: If performance is good enough we could also try to skip chars in query so `hread` could be matched with `head`
recursiveSearch :: Text
    -> Tree.Node
    -> MatchKind
    -> Match
    -> Int
    -> Map Index Result
    -> Map Index Result
recursiveSearch query node matchKind matched pos scoreMap' = do
    let resultKind = if Text.null query then matchKind else NotFullyMatched
        result     = Result resultKind matched 0
        scoreMap   = insertResult (node ^. Tree.index) result scoreMap'
        caseInsensitiveEquality = min matchKind CaseInsensitiveEquality
        mayUnconsQuery          = Text.uncons query
        matchWithHead h t newMatchKind sMap = matchQueryHead
            h t node newMatchKind matched pos sMap
        matchHead h t
            | not $ isLetter h = matchWithHead h t matchKind scoreMap
            | isUpper h
                = matchWithHead (toLower h) t caseInsensitiveEquality
                $ matchWithHead h t matchKind scoreMap
            | otherwise
                = matchWithHead (toUpper h) t caseInsensitiveEquality
                $ matchWithHead h t matchKind scoreMap
        updatedMap = maybe scoreMap (uncurry matchHead) mayUnconsQuery
    skipDataHead query node matched pos updatedMap


insertResult :: Index -> Result -> Map Index Result -> Map Index Result
insertResult i r m = if Index.isInvalid i then m else Map.insertWith max i r m
{-# INLINE insertResult #-}

skipDataHead :: Text
    -> Tree.Node
    -> Match
    -> Int
    -> Map Index Result
    -> Map Index Result
skipDataHead query node matched pos scoreMap = Map.foldl
    (\acc n -> recursiveSearch query n AllCharsMatched matched pos acc)
    scoreMap
    $ node ^. Tree.branches

matchQueryHead :: Char
    -> Text
    -> Tree.Node
    -> MatchKind
    -> Match
    -> Int
    -> Map Index Result
    -> Map Index Result
matchQueryHead qHead qSuffix node matchKind matched pos scoreMap =
    let mayMatchedNode = node ^. Tree.branches . at qHead
        newPos         = pos + 1
        newRange       = Match.addPosition pos matched
        processSuffix n
            = recursiveSearch qSuffix n matchKind newRange newPos scoreMap
    in maybe scoreMap processSuffix mayMatchedNode
