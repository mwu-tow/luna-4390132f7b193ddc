{-# LANGUAGE Strict #-}

module Searcher.Engine.SearchSpec (spec) where

import Prologue hiding (Index)
import Test.Hspec

import qualified Data.List                      as List
import qualified Data.Map.Strict                as Map
import qualified Searcher.Engine.Data.Database  as Database
import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Result    as Result
import qualified Searcher.Engine.Data.Substring as Substring
import qualified Searcher.Engine.Data.Tree      as Tree
import qualified Searcher.Engine.Search         as Search

import Data.Map.Strict                      (Map)
import Searcher.Engine.Data.Database        (Database, SearcherData)
import Searcher.Engine.Data.Index           (Index)
import Searcher.Engine.Data.Match           (Match)
import Searcher.Engine.Data.Result          (Result)
import Searcher.Engine.Metric.DefaultMetric (DefaultMetric)



-------------------
-- === Utils === --
-------------------

defSearch :: SearcherData a => Text -> Database a -> [Result a]
defSearch = \query database ->
    Search.search query database (const 1) (def @DefaultMetric)
{-# INLINE defSearch #-}

defMatchQuery :: Text -> Tree.Root -> (Map Index Match)
defMatchQuery = \query database ->
    Search.matchQuery query database (def @DefaultMetric)
{-# INLINE defMatchQuery #-}

defUpdateValue :: Tree.Node -> Match.State -> Map Index Match
    -> (Map Index Match)
defUpdateValue = \node state resultMap ->
    Search.updateValue node state resultMap (def @DefaultMetric)
{-# INLINE defUpdateValue #-}

topResultNameShouldBe :: [Result Text] -> Text -> Expectation
topResultNameShouldBe []    _ = expectationFailure "Result is empty"
topResultNameShouldBe (h:_) r = h ^. Result.hint `shouldBe` r
{-# INLINE topResultNameShouldBe #-}



-------------------
-- === Tests === --
-------------------

spec :: Spec
spec = do
    describe "updateValue function" $ do
        it "map is updated" $ let
            node       = Tree.Node 0 mempty
            state      = Match.mkState def
            updatedMap = defUpdateValue node state mempty
            in updatedMap `shouldBe` Map.singleton 0 def
        it "match kind is correct" $ let
            node         = Tree.Node 0 mempty
            state        = Match.mkState def
            scoreMap     = defUpdateValue node state mempty
            mayMatch     = Map.lookup 0 scoreMap
            mayMatchKind = view Match.kind <$> mayMatch
            in mayMatchKind `shouldBe` Just Substring.Equal

    describe "matchQuery function" $ do
        it "all values from tree are in map" $ let
            input :: [Text]
            input     = ["aa", "ab"]
            database  = Database.mk input
            root      = database ^. Database.tree
            hints'    = database ^. Database.hints
            resultMap = defMatchQuery mempty root
            in Map.keys hints' `shouldMatchList` Map.keys resultMap
        it "case sensitive is better than insensitive" $ let
            input :: [Text]
            input = ["bar", "Bar"]
            database = Database.mk input
            root     = database ^. Database.tree
            results  = defMatchQuery "bar" root
            maxIdx   = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 0
        it "equality is better then matching" $ let
            input :: [Text]
            input = ["baru", "Bar"]
            database = Database.mk input
            root     = database ^. Database.tree
            results  = defMatchQuery "bar" root
            maxIdx   = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 1
        it "matching all is better than not" $ let
            input :: [Text]
            input = ["abc", "adc"]
            database = Database.mk input
            root     = database ^. Database.tree
            results  = defMatchQuery "ab" root
            maxIdx   = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 0

    describe "search with default metrics" $ do
        it "match starting with capital is prefered over others" $ let
            input :: [Text]
            input  = ["fooBar", "optbaru"]
            result = defSearch "bar" $! Database.mk input
            in result `topResultNameShouldBe` "fooBar"
        it "subsequence should be prefered over substring" $ let
            input :: [Text]
            input  = ["abcdef", "aebdcf"]
            result = defSearch "abc" $! Database.mk input
            in result `topResultNameShouldBe` "abcdef"
        it "subsequence at beggining of the word is prefered over other subsequences" $ let
            input :: [Text]
            input  = ["getElement", "delement"]
            result = defSearch "ele" $! Database.mk input
            in result `topResultNameShouldBe` "getElement"
        it "subsequence at beggining of the word is prefered over other subsequences" $ let
            input :: [Text]
            input  = ["abxcdefx", "aebdcfx"]
            result = defSearch "abcf" $! Database.mk input
            in result `topResultNameShouldBe` "abxcdefx"
        it "last letter match should be prefered when results are similar" $ let
            input :: [Text]
            input  = ["xxaaxx", "xxbbxxb"]
            result = defSearch "xxxx" $! Database.mk input
            in result `topResultNameShouldBe` "xxaaxx"
        it "longer subsequence is favored" $ let
            input :: [Text]
            input  = ["axxaxxax", "bxbxxxxb"]
            result = defSearch "xxxxx" $! Database.mk input
            in result `topResultNameShouldBe` "bxbxxxxb"
        it "result with less omitted letters is prefered" $ let
            input :: [Text]
            input  = ["xxaxxxa", "xxbbxxxb"]
            result = defSearch "xxxxx" $! Database.mk input
            in result `topResultNameShouldBe` "xxaxxxa"
        it "exact match is prefered" $ let
            input :: [Text]
            input  = ["streamFrom", "stream"]
            result = defSearch "stream" $! Database.mk input
            in result `topResultNameShouldBe` "stream"
        it "exact match is prefered when case differs" $ let
            input :: [Text]
            input  = ["fooBar", "foobar"]
            result = defSearch "foobar" $! Database.mk input
            in result `topResultNameShouldBe` "foobar"
        -- it "methods matching class are first for empty query" $ do
        --     let bestEntry = Symbol "%" (Library def True) (Method "Int")  def 0.7 def
        --         entries = [ Symbol "+" (Library def True) (Method "Int")  def 0.7 def
        --                     , Symbol "+" (Library def True) (Method "Text") def 0.5 def
        --                     , Symbol "%" (Library def True) (Method "Text") def 0.5 def
        --                     , Symbol "+" (Library def True) Function        def 0.3 def
        --                     , Symbol "%" (Library def True) Function        def 0.3 def
        --                     , Symbol "+" (Library def True) Function        def 0.2 def
        --                     , bestEntry
        --                     ]
        --         res = search "" entries
        --     res `topResultEntryShouldBe` bestEntry

        describe "matched letters" $ do
            it "match should be eager" $ let
                input :: [Text]
                input    = ["xx"]
                [result] = defSearch "x" $! Database.mk input
                revRange = result ^. Result.match . Match.substring
                    . Substring.reversedRange
                expected = pure $! Substring.Range 0 1
                in revRange `shouldBe` expected
            it "match should be eager" $ let
                input :: [Text]
                input    = ["xxx"]
                [result] = defSearch "xx" $! Database.mk input
                revRange = result ^. Result.match . Match.substring
                    . Substring.reversedRange
                expected = pure $! Substring.Range 0 2
                in revRange `shouldBe` expected

