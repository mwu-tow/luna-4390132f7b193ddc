{-# LANGUAGE Strict #-}

module Searcher.Engine.Search where

import Prologue hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Data.List                   as List
import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as Text
import qualified Searcher.Engine.Data.Database    as Database
import qualified Searcher.Engine.Data.Index       as Index
import qualified Searcher.Engine.Data.Match       as Match
import qualified Searcher.Engine.Data.Substring   as Substring
import qualified Searcher.Engine.Data.Tree        as Tree
import qualified Searcher.Engine.Metric           as Metric

import Data.Char                         (isLetter, isUpper, toLower, toUpper)
import Data.Map.Strict                   (Map)
import Searcher.Engine.Data.Database          (Database, SearcherData)
import Searcher.Engine.Data.Index             (Index)
import Searcher.Engine.Data.Match             (Match (Match))
import Searcher.Engine.Data.Result            (Result (Result))
import Searcher.Engine.Metric     (updateMetrics, getMetrics)
import Searcher.Engine.Metric.PrefixBonus     (PrefixBonus)
import Searcher.Engine.Metric.SequenceBonus   (SequenceBonus)
import Searcher.Engine.Metric.MismatchPenalty     (MismatchPenalty)
import Searcher.Engine.Metric.SuffixBonus     (SuffixBonus)
import Searcher.Engine.Metric.WordPrefixBonus (WordPrefixBonus)
import Searcher.Engine.Metric.WordSuffixBonus (WordSuffixBonus)



--------------------
-- === Search === --
--------------------


-- === API === --

search :: forall m a . (Metric.MonadMetrics DefaultMetrics m , SearcherData a)
    => Text -> Database a -> m [Result a]
search query database = do
    let root    = database ^. Database.tree
        hints   = database ^. Database.hints
    matches <- matchQuery query root
    let results = concat $! Map.elems $! toResultMap hints matches
    pure $! List.sort results
{-# INLINE search #-}


-- === Utils === --

-- [TODO]: DefaultMetrics in all signatures should go back to ss once 
-- we figure out how to do transaction without get and put
transaction :: forall m a . Metric.MonadMetrics DefaultMetrics m
    => m a -> m a
transaction action = do
    mismatchPenalty <- State.get @MismatchPenalty
    prefixBonus     <- State.get @PrefixBonus
    sequenceBonus   <- State.get @SequenceBonus
    suffixBonus     <- State.get @SuffixBonus
    wordPrefixBonus <- State.get @WordPrefixBonus
    wordSuffixBonus <- State.get @WordSuffixBonus
    result <- action
    State.put @MismatchPenalty mismatchPenalty
    State.put @PrefixBonus     prefixBonus
    State.put @SequenceBonus   sequenceBonus
    State.put @SuffixBonus     suffixBonus
    State.put @WordPrefixBonus wordPrefixBonus
    State.put @WordSuffixBonus wordSuffixBonus
    pure result

toResultMap :: Map Index [a] -> Map Index Match -> Map Index [Result a]
toResultMap hintsMap matchesMap = let
    toResults hints match = (\h -> Result h match) <$> hints
    in Map.intersectionWith toResults hintsMap matchesMap
{-# INLINE toResultMap #-}


matchQuery :: forall m . Metric.MonadMetrics DefaultMetrics m
    => Text -> Tree.Root -> m (Map Index Match)
matchQuery query root = recursiveMatchQuery root state mempty where
    state = Match.mkState query
{-# INLINE matchQuery #-}

-- TODO [LJK]: If performance is good enough we could also try to skip chars in
-- query so `hread` could be matched with `head`
-- [Ara] This should only come into play if there are no matches for a given
-- query.
recursiveMatchQuery :: forall m . Metric.MonadMetrics DefaultMetrics m
    => Tree.Node
    -> Match.State
    -> Map Index Match
    -> m (Map Index Match)
recursiveMatchQuery node state scoreMap
    =   skipDataHead   node state
    =<< matchQueryHead node state
    =<< updateValue    node state scoreMap

updateValue :: forall m . Metric.MonadMetrics DefaultMetrics m
    => Tree.Node
    -> Match.State
    -> Map Index Match
    -> m (Map Index Match)
updateValue node state scoreMap = do
    let idx          = node  ^. Tree.index
        suffix       = state ^. Match.remainingSuffix
        substring    = state ^. Match.currentSubstring
        kind         = state ^. Match.currentKind
        kind'        = if Text.null suffix then kind else Substring.Other
        updatedState = state & Match.currentKind .~ kind'
    if Index.isInvalid idx then pure scoreMap else do
        score <- getMetrics @DefaultMetrics updatedState
        let match = Match substring kind' score
        pure $! insertMatch idx match scoreMap
{-# INLINE updateValue #-}

insertMatch :: Index -> Match -> Map Index Match -> Map Index Match
insertMatch i r m = if Index.isInvalid i then m else Map.insertWith max i r m
{-# INLINE insertMatch #-}

skipDataHead :: forall m . Metric.MonadMetrics DefaultMetrics m
    => Tree.Node
    -> Match.State
    -> Map Index Match
    -> m (Map Index Match)
skipDataHead node state scoreMap = let
    updatedState = state
        & Match.currentKind .~ Substring.FullMatch
        & Match.positionInData %~ (+1)
    skipChar = \acc (c, n) -> transaction $! do
        updateMetrics @DefaultMetrics c Match.NotMatched updatedState
        recursiveMatchQuery n updatedState acc
    in foldlM skipChar scoreMap $! toList $! node ^. Tree.branches

matchQueryHead :: forall m . Metric.MonadMetrics DefaultMetrics m
    => Tree.Node
    -> Match.State
    -> Map Index Match
    -> m (Map Index Match)
matchQueryHead node state scoreMap = let
    suffix = state ^. Match.remainingSuffix
    in case Text.uncons suffix of
        Nothing            -> pure scoreMap
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

            matchCaseSensitive 
                :: forall m . Metric.MonadMetrics DefaultMetrics m
                => Map Index Match -> m (Map Index Match)
            matchCaseSensitive = \scoreMap' -> transaction $! do
                updateMetrics @DefaultMetrics h Match.Equal caseSensitiveState
                maybe
                    (pure scoreMap')
                    (\n -> recursiveMatchQuery n caseSensitiveState scoreMap')
                    mayCaseSensitiveNextNode
            {-# INLINEABLE matchCaseSensitive #-}

            matchCaseInsensitive 
                :: forall m . Metric.MonadMetrics DefaultMetrics m
                => Map Index Match -> m (Map Index Match)
            matchCaseInsensitive = \scoreMap' -> transaction $! do
                updateMetrics @DefaultMetrics
                    counterCaseH Match.CaseInsensitive caseInsensitiveState
                maybe
                    (pure scoreMap')
                    (\n -> recursiveMatchQuery n caseInsensitiveState scoreMap')
                    mayCaseInsensitiveNextNode
            {-# INLINEABLE matchCaseInsensitive #-}

            defMatchers :: forall m . Metric.MonadMetrics DefaultMetrics m
                => [Map Index Match -> m (Map Index Match)]
            defMatchers = [matchCaseSensitive]
            {-# INLINEABLE defMatchers #-}

            extraMatchers :: forall m . Metric.MonadMetrics DefaultMetrics m
                => [Map Index Match -> m (Map Index Match)]
            extraMatchers = [matchCaseInsensitive]
            {-# INLINEABLE extraMatchers #-}

            matchers :: forall m . Metric.MonadMetrics DefaultMetrics m
                => [Map Index Match -> m (Map Index Match)]
            matchers = defMatchers <> if isLetter h then extraMatchers else []
            {-# INLINEABLE matchers #-}
            -- END --
            in foldlM (\acc matcher -> matcher acc) scoreMap $! matchers

type DefaultMetrics =
    '[ MismatchPenalty
    , PrefixBonus
    , SequenceBonus
    , SuffixBonus
    , WordPrefixBonus
    , WordSuffixBonus ]

test :: [Result Text]
test = runIdentity
    $! State.evalDefT @WordSuffixBonus
    .  State.evalDefT @WordPrefixBonus
    .  State.evalDefT @SuffixBonus
    .  State.evalDefT @SequenceBonus
    .  State.evalDefT @PrefixBonus
    .  State.evalDefT @MismatchPenalty
    $! search
        "Tst"
        $ Database.mk ["Test", "Testing", "Tester", "Foo", "Foot"]

