{-# LANGUAGE Strict #-}

module Searcher.Engine.Search where

import Prologue hiding (Index)

import qualified Data.List                      as List
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as Text
import qualified Searcher.Engine.Data.Database  as Database
import qualified Searcher.Engine.Data.Index     as Index
import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Result    as Result
import qualified Searcher.Engine.Data.Substring as Substring
import qualified Searcher.Engine.Data.Tree      as Tree
import qualified Searcher.Engine.Metric         as Metric

import Data.Char                     (isLetter, isUpper, toLower, toUpper)
import Data.Map.Strict               (Map)
import Searcher.Engine.Data.Database (Database, SearcherData)
import Searcher.Engine.Data.Index    (Index)
import Searcher.Engine.Data.Match    (Match (Match))
import Searcher.Engine.Data.Result   (Result (Result))
import Searcher.Engine.Metric        (Metric)

-- Pre-integration perf for matchQuery ~= 70ms



--------------------
-- === Search === --
--------------------

-- === API === --

search :: forall ts a . ( SearcherData a, Metric.MakeDefault ts, Metric.Edit ts
                        , Metric.States ts )
    => Text -> Database a -> (a -> Double) -> [Result a]
search = \query database hintWeightGetter ->
    searchWith query database hintWeightGetter (Metric.make @ts)
{-# INLINE search #-}

searchWith :: forall a ts . (SearcherData a, Metric.Edit ts, Metric.States ts)
    => Text -> Database a -> (a -> Double) -> Metric ts -> [Result a]
searchWith = \query database hintWeightGetter metric -> let
    root    = database ^. Database.tree
    hints   = database ^. Database.hints
    matches = matchQuery query root metric
    results  = concat $! Map.elems $! toResultMap hints matches
    getScore = Result.getScore hintWeightGetter
    compareRes r1 r2 = getScore r2 `compare` getScore r1
    in List.sortBy compareRes results
{-# INLINE searchWith #-}


-- === Utils === --

toResultMap :: Map Index [a] -> Map Index Match -> Map Index [Result a]
toResultMap hintsMap matchesMap = let
    toResults hints match = (\h -> Result h match) <$> hints
    in Map.intersectionWith toResults hintsMap matchesMap
{-# INLINE toResultMap #-}

matchQuery :: (Metric.Edit ts, Metric.States ts) => Text -> Tree.Root
    -> Metric ts -> Map Index Match
matchQuery = \query root metric ->
    recursiveMatchQuery root (Match.mkState query) mempty metric
{-# INLINE matchQuery #-}

-- TODO [LJK]: If performance is good enough we could also try to skip chars in
-- query so `hread` could be matched with `head`
-- [Ara] This should only come into play if there are no matches for a given
-- query.
recursiveMatchQuery :: (Metric.Edit ts, Metric.States ts) => Tree.Node
    -> Match.State -> Map Index Match -> Metric ts -> Map Index Match
recursiveMatchQuery = \node state scoreMap metric -> let
    scores = matchQueryHead node state vals metric
    vals   = updateValue node state scoreMap metric
    in skipDataHead node state scores metric

updateValue :: (Metric.Get ts, Metric.States ts) => Tree.Node -> Match.State
    -> Map Index Match -> Metric ts -> Map Index Match
updateValue = \node state scoreMap matchSt -> let
    idx          = node  ^. Tree.index
    suffix       = state ^. Match.remainingSuffix
    substring    = state ^. Match.currentSubstring
    kind         = state ^. Match.currentKind
    kind'        = if Text.null suffix then kind else Substring.Other
    updatedState = state & Match.currentKind .~ kind'
    in if Index.isInvalid idx then scoreMap else let
        score = Metric.get matchSt updatedState
        match = Match substring kind' score
        in insertMatch idx match scoreMap
{-# INLINE updateValue #-}

insertMatch :: Index -> Match -> Map Index Match -> Map Index Match
insertMatch i r m = if Index.isInvalid i then m else Map.insertWith max i r m
{-# INLINE insertMatch #-}

skipDataHead :: (Metric.Edit ts, Metric.States ts) => Tree.Node -> Match.State
    -> Map Index Match -> Metric ts -> Map Index Match
skipDataHead = \node state scoreMap metric -> let
    updatedState = state
        & Match.currentKind    .~ Substring.FullMatch
        & Match.positionInData %~ (+1)
    skipChar = \acc (!c, !n) -> let
        newMetSt = Metric.update metric c Match.NotMatched updatedState
        in recursiveMatchQuery n updatedState acc newMetSt
    in foldl' skipChar scoreMap $! toList $! node ^. Tree.branches

matchQueryHead :: (Metric.Edit ts, Metric.States ts) => Tree.Node
    -> Match.State -> Map Index Match -> Metric ts -> Map Index Match
matchQueryHead = \node state scoreMap metricSt -> let
    suffix = state ^. Match.remainingSuffix
    in case Text.uncons suffix of
        Nothing           -> scoreMap
        Just ((!h), (!t)) -> let
            branches     = node ^. Tree.branches
            -- This part should be lazy
            -- START --
            counterCaseH = if isUpper h then toLower h else toUpper h
            mayCaseSensitiveNextNode   = Map.lookup h branches
            mayCaseInsensitiveNextNode = Map.lookup counterCaseH branches
            updateKind = \prev -> min prev Substring.CaseInsensitiveEqual
            posInData  = state ^. Match.positionInData
            substring  = state ^. Match.currentSubstring
            updatedSubstring = Substring.addPosition posInData substring
            caseSensitiveState = state
                & Match.remainingSuffix  .~ t
                & Match.positionInData   %~ (+1)
                & Match.positionInQuery  %~ (+1)
                & Match.currentSubstring .~ updatedSubstring
            caseInsensitiveState = caseSensitiveState
                & Match.currentKind %~ updateKind

            matchCaseSensitive :: Map Index Match -> Map Index Match
            matchCaseSensitive = \scoreMap' -> let
                newMetSt = Metric.update metricSt h Match.Equal
                    caseSensitiveState
                in maybe scoreMap'
                    (\n -> recursiveMatchQuery n caseSensitiveState scoreMap'
                        newMetSt)
                    mayCaseSensitiveNextNode
            {-# INLINEABLE matchCaseSensitive #-}

            matchCaseInsensitive :: Map Index Match -> Map Index Match
            matchCaseInsensitive = \scoreMap' -> let
                newMetSt = Metric.update metricSt counterCaseH
                    Match.CaseInsensitive caseInsensitiveState
                in maybe scoreMap'
                    (\n -> recursiveMatchQuery n caseInsensitiveState scoreMap'
                        newMetSt)
                    mayCaseInsensitiveNextNode
            {-# INLINEABLE matchCaseInsensitive #-}

            defMatchers :: [Map Index Match -> Map Index Match]
            defMatchers = [matchCaseSensitive]
            {-# INLINEABLE defMatchers #-}

            extraMatchers :: [Map Index Match -> Map Index Match]
            extraMatchers = [matchCaseInsensitive]
            {-# INLINEABLE extraMatchers #-}

            matchers :: [Map Index Match -> Map Index Match]
            matchers = defMatchers <> if isLetter h then extraMatchers else []
            {-# INLINEABLE matchers #-}
            -- END --
            in foldl' (\acc matcher -> matcher acc) scoreMap $! matchers

