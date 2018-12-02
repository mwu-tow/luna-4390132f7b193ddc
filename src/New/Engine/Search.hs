{-# LANGUAGE Strict #-}

module New.Engine.Search where

import Prologue hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Data.List                   as List
import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Database    as Database
import qualified New.Engine.Data.Index       as Index
import qualified New.Engine.Data.Match       as Match
import qualified New.Engine.Data.Substring   as Substring
import qualified New.Engine.Data.Tree        as Tree
import qualified New.Engine.Metric           as Metric

import Data.Char                         (isLetter, isUpper, toLower, toUpper)
import Data.Map.Strict                   (Map)
import New.Engine.Data.Database          (Database, SearcherData)
import New.Engine.Data.Index             (Index)
import New.Engine.Data.Match             (Match (Match))
import New.Engine.Data.Result            (Result (Result))
import New.Engine.Metric.PrefixBonus     (PrefixBonus)
import New.Engine.Metric.SequenceBonus   (SequenceBonus)
import New.Engine.Metric.SkipPenalty     (SkipPenalty)
import New.Engine.Metric.SuffixBonus     (SuffixBonus)
import New.Engine.Metric.WordPrefixBonus (WordPrefixBonus)
import New.Engine.Metric.WordSuffixBonus (WordSuffixBonus)



--------------------
-- === Search === --
--------------------


-- === API === --

search :: forall ss m a . (Metric.MonadMetrics ss m , SearcherData a)
    => Text -> Database a -> m [Result a]
search query database = do
    let root    = database ^. Database.tree
        hints   = database ^. Database.hints
    matches <- matchQuery @ss query root
    let results = concat $! Map.elems $! toResultMap hints matches
    pure $! List.sort results
{-# INLINE search #-}


-- === Utils === --

toResultMap :: Map Index [a] -> Map Index Match -> Map Index [Result a]
toResultMap hintsMap matchesMap = let
    toResults hints match = (\h -> Result h match) <$> hints
    in Map.intersectionWith toResults hintsMap matchesMap
{-# INLINE toResultMap #-}


matchQuery :: forall ss m . Metric.MonadMetrics ss m
    => Text -> Tree.Root -> m (Map Index Match)
matchQuery query root = recursiveMatchQuery @ss root state mempty where
    state = Match.mkState query
{-# INLINE matchQuery #-}

-- TODO [LJK]: If performance is good enough we could also try to skip chars in
-- query so `hread` could be matched with `head`
-- [Ara] This should only come into play if there are no matches for a given
-- query.
recursiveMatchQuery :: forall ss m . Metric.MonadMetrics ss m
    => Tree.Node
    -> Match.State
    -> Map Index Match
    -> m (Map Index Match)
recursiveMatchQuery node state scoreMap
    =   skipDataHead   @ss node state
    =<< matchQueryHead @ss node state
    =<< updateValue    @ss node state scoreMap

updateValue :: forall ss m . Metric.MonadMetrics ss m
    => Tree.Node
    -> Match.State
    -> Map Index Match
    -> m (Map Index Match)
updateValue node state scoreMap = let
    idx       = node  ^. Tree.index
    suffix    = state ^. Match.remainingSuffix
    substring = state ^. Match.currentSubstring
    kind      = state ^. Match.currentKind
    kind'     = if Text.null suffix then kind else Substring.Other
    match     = Match substring kind' def
    in pure $! insertMatch idx match scoreMap
{-# INLINE updateValue #-}

insertMatch :: Index -> Match -> Map Index Match -> Map Index Match
insertMatch i r m = if Index.isInvalid i then m else Map.insertWith max i r m
{-# INLINE insertMatch #-}

skipDataHead :: forall ss m . Metric.MonadMetrics ss m
    => Tree.Node
    -> Match.State
    -> Map Index Match
    -> m (Map Index Match)
skipDataHead node state scoreMap = let
    updatedState = state
        & Match.currentKind .~ Substring.FullMatch
        & Match.positionInData %~ (+1)
    in foldlM
        (\acc n -> recursiveMatchQuery @ss n updatedState acc)
        scoreMap
        $! node ^. Tree.branches

matchQueryHead :: forall ss m . Metric.MonadMetrics ss m
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

            matchCaseSensitive :: forall ss m . Metric.MonadMetrics ss m
                => Map Index Match -> m (Map Index Match)
            matchCaseSensitive = \scoreMap' -> maybe
                (pure scoreMap')
                (\n -> recursiveMatchQuery @ss n caseSensitiveState scoreMap')
                mayCaseSensitiveNextNode
            {-# INLINEABLE matchCaseSensitive #-}

            matchCaseInsensitive :: forall ss m . Metric.MonadMetrics ss m
                => Map Index Match -> m (Map Index Match)
            matchCaseInsensitive = \scoreMap' -> maybe
                (pure scoreMap')
                (\n -> recursiveMatchQuery @ss n caseInsensitiveState scoreMap')
                mayCaseInsensitiveNextNode
            {-# INLINEABLE matchCaseInsensitive #-}

            defMatchers :: forall ss m . Metric.MonadMetrics ss m
                => [Map Index Match -> m (Map Index Match)]
            defMatchers = [matchCaseSensitive @ss]
            {-# INLINEABLE defMatchers #-}

            extraMatchers :: forall ss m . Metric.MonadMetrics ss m
                => [Map Index Match -> m (Map Index Match)]
            extraMatchers = [matchCaseInsensitive @ss]
            {-# INLINEABLE extraMatchers #-}

            matchers :: forall ss m . Metric.MonadMetrics ss m
                => [Map Index Match -> m (Map Index Match)]
            matchers = defMatchers @ss <> if isLetter h then extraMatchers @ss else []
            {-# INLINEABLE matchers #-}
            -- END --
            in foldlM (\acc matcher -> matcher acc) scoreMap $! matchers @ss

type DefaultMetrics =
    '[ PrefixBonus
    , SequenceBonus
    , SkipPenalty
    , SuffixBonus
    , WordPrefixBonus
    , WordSuffixBonus ]

test :: [Result Text]
test = runIdentity
    $! State.evalDefT @WordSuffixBonus
    .  State.evalDefT @WordPrefixBonus
    .  State.evalDefT @SuffixBonus
    .  State.evalDefT @SkipPenalty
    .  State.evalDefT @SequenceBonus
    .  State.evalDefT @PrefixBonus
    $! search
        @DefaultMetrics
        "Tst"
        $ Database.mk ["Test", "Testing", "Tester", "Foo", "Foot"]

