{-# LANGUAGE Strict #-}
module New.Engine.Search where

import Prologue hiding (Index)

import qualified Data.List           as List
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as Text
import qualified New.Engine.Data.Database  as Database
import qualified New.Engine.Data.Index     as Index
import qualified New.Engine.Data.Substring as Substring
import qualified New.Engine.Data.Tree      as Tree

import Data.Char                 (isLetter, isUpper, toLower, toUpper)
import Data.Map.Strict           (Map)
import New.Engine.Data.Database  (SearcherData, Database)
import New.Engine.Data.Index     (Index)
import New.Engine.Data.Result    (Match (Match), Result (Result))
import New.Engine.Data.Substring (Substring)


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
matchQuery query root = let
    initPosition = 0
    equality     = Substring.Equal
    in recursiveMatchQuery query root equality mempty initPosition mempty
{-# INLINE matchQuery #-}

-- TODO [LJK]: If performance is good enough we could also try to skip chars in
-- query so `hread` could be matched with `head`
-- [Ara] This should only come into play if there are no matches for a given
-- query.
recursiveMatchQuery :: Text
    -> Tree.Node
    -> Substring.Kind
    -> Substring
    -> Int
    -> Map Index Match
    -> Map Index Match
recursiveMatchQuery query node sKind matched pos scoreMap = do
    let mayUnconsQuery    = Text.uncons query
        caseInsensitiveEq = min sKind Substring.CaseInsensitiveEqual
        matchWithHead h t newMatchKind sMap = matchQueryHead
            h t node newMatchKind matched pos sMap
        matchWithLetter h t = let
            counterCase = if isUpper h then toLower h else toUpper h
            in matchWithHead counterCase t caseInsensitiveEq
                $ matchWithHead h t sKind scoreMap'
        matchHead h t
            | isLetter h = matchWithLetter h t
            | otherwise  = matchWithHead   h t sKind scoreMap'
        scoreMap'  = updateValue query node sKind matched scoreMap
        updatedMap = maybe scoreMap' (uncurry matchHead) mayUnconsQuery
    skipDataHead query node matched pos updatedMap

updateValue :: Text
    -> Tree.Node
    -> Substring.Kind
    -> Substring
    -> Map Index Match
    -> Map Index Match
updateValue suffix node sKind matched scoreMap = let
    idx    = node ^. Tree.index
    sKind' = if Text.null suffix then sKind else Substring.Other
    match  = Match matched sKind' 0
    in insertMatch idx match scoreMap
{-# INLINE updateValue #-}

insertMatch :: Index -> Match -> Map Index Match -> Map Index Match
insertMatch i r m = if Index.isInvalid i then m else Map.insertWith max i r m
{-# INLINE insertMatch #-}

skipDataHead :: Text
    -> Tree.Node
    -> Substring
    -> Int
    -> Map Index Match
    -> Map Index Match
skipDataHead query node matched pos scoreMap = Map.foldl
    (\acc n -> recursiveMatchQuery query n Substring.FullMatch matched pos acc)
    scoreMap
    $! node ^. Tree.branches

matchQueryHead :: Char
    -> Text
    -> Tree.Node
    -> Substring.Kind
    -> Substring
    -> Int
    -> Map Index Match
    -> Map Index Match
matchQueryHead qHead qSuffix node sKind matched pos scoreMap =
    let mayMatchedNode = node ^. Tree.branches . at qHead
        newPos         = pos + 1
        newRange       = Substring.addPosition pos matched
        processSuffix n
            = recursiveMatchQuery qSuffix n sKind newRange newPos scoreMap
    in maybe scoreMap processSuffix mayMatchedNode

test :: [Result Text]
test = search "Tst" $ Database.mk ["Test", "Testing", "Tester", "Foo", "Foot"]

