{-# LANGUAGE Strict #-}

module New.Engine.Search where

import Prologue hiding (Index)

import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as Text
import qualified New.Engine.Data.Database  as Database
import qualified New.Engine.Data.Index     as Index
import qualified New.Engine.Data.Match     as Match
import qualified New.Engine.Data.Substring as Substring
import qualified New.Engine.Data.Tree      as Tree

import Data.Char                (isLetter, isUpper, toLower, toUpper)
import Data.Map.Strict          (Map)
import New.Engine.Data.Database (Database, SearcherData)
import New.Engine.Data.Index    (Index)
import New.Engine.Data.Match    (Match (Match))
import New.Engine.Data.Result   (Result (Result))



--------------------
-- === Search === --
--------------------

-- === API === --

search :: SearcherData a => Text -> Database a -> [Result a]
search query database = let
    root    = database ^. Database.tree
    hints   = database ^. Database.hints
    matches = matchQuery query root
    in List.sort $! concat $! Map.elems $! toResultMap hints matches
{-# INLINE search #-}


-- === Utils === --

toResultMap :: Map Index [a] -> Map Index Match -> Map Index [Result a]
toResultMap hintsMap matchesMap = let
    toResults hints match = (\h -> Result h match) <$> hints
    in Map.intersectionWith toResults hintsMap matchesMap
{-# INLINE toResultMap #-}


matchQuery :: Text -> Tree.Root -> Map Index Match
matchQuery query root = recursiveMatchQuery root state mempty where
    state = Match.mkState query
{-# INLINE matchQuery #-}

-- TODO [LJK]: If performance is good enough we could also try to skip chars in
-- query so `hread` could be matched with `head`
-- [Ara] This should only come into play if there are no matches for a given
-- query.
recursiveMatchQuery :: Tree.Node
    -> Match.State
    -> Map Index Match
    -> Map Index Match
recursiveMatchQuery node state scoreMap
    =  skipDataHead   node state
    $! matchQueryHead node state
    $! updateValue    node state scoreMap

updateValue :: Tree.Node
    -> Match.State
    -> Map Index Match
    -> Map Index Match
updateValue node state scoreMap = let
    idx       = node  ^. Tree.index
    suffix    = state ^. Match.remainingSuffix
    substring = state ^. Match.currentSubstring
    kind      = state ^. Match.currentKind
    kind'     = if Text.null suffix then kind else Substring.Other
    match     = Match substring kind' def
    in insertMatch idx match scoreMap
{-# INLINE updateValue #-}

insertMatch :: Index -> Match -> Map Index Match -> Map Index Match
insertMatch i r m = if Index.isInvalid i then m else Map.insertWith max i r m
{-# INLINE insertMatch #-}

skipDataHead :: Tree.Node
    -> Match.State
    -> Map Index Match
    -> Map Index Match
skipDataHead node state scoreMap = let
    updatedState = state
        & Match.currentKind .~ Substring.FullMatch
        & Match.positionInData %~ (+1)
    in Map.foldl
        (\acc n -> recursiveMatchQuery n updatedState acc)
        scoreMap
        $! node ^. Tree.branches

matchQueryHead :: Tree.Node
    -> Match.State
    -> Map Index Match
    -> Map Index Match
matchQueryHead node state scoreMap = let
    suffix = state ^. Match.remainingSuffix
    in case Text.uncons suffix of
        Nothing            -> scoreMap
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
            matchCaseSensitive = \scoreMap' -> maybe
                scoreMap'
                (\n -> recursiveMatchQuery n caseSensitiveState scoreMap')
                mayCaseSensitiveNextNode
            {-# INLINEABLE matchCaseSensitive #-}

            matchCaseInsensitive :: Map Index Match -> Map Index Match
            matchCaseInsensitive = \scoreMap' -> maybe
                scoreMap'
                (\n -> recursiveMatchQuery n caseInsensitiveState scoreMap')
                mayCaseInsensitiveNextNode
            {-# INLINEABLE matchCaseInsensitive #-}

            defMatchers :: [Map Index Match -> Map Index Match]
            defMatchers   = [matchCaseSensitive]
            {-# INLINEABLE defMatchers #-}

            extraMatchers :: [Map Index Match -> Map Index Match]
            extraMatchers = [matchCaseInsensitive]
            {-# INLINEABLE extraMatchers #-}

            matchers :: [Map Index Match -> Map Index Match]
            matchers = defMatchers <> if isLetter h then extraMatchers else []
            {-# INLINEABLE matchers #-}
            -- END --
            in foldl' (\acc matcher -> matcher acc) scoreMap matchers

test :: [Result Text]
test = search "Tst" $ Database.mk ["Test", "Testing", "Tester", "Foo", "Foot"]

