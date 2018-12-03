module New.Engine.SearchSpec (spec) where

import Prologue   hiding (Index)
import Test.Hspec

import qualified Control.Monad.State.Layered as State
import qualified Data.List                   as List
import qualified Data.Map.Strict             as Map
import qualified New.Engine.Data.Database    as Database
import qualified New.Engine.Data.Match       as Match
import qualified New.Engine.Data.Substring   as Substring
import qualified New.Engine.Data.Tree        as Tree
import qualified New.Engine.Search           as Search

import New.Engine.Metric.PrefixBonus     (PrefixBonus)
import New.Engine.Metric.SequenceBonus   (SequenceBonus)
import New.Engine.Metric.SkipPenalty     (SkipPenalty)
import New.Engine.Metric.SuffixBonus     (SuffixBonus)
import New.Engine.Metric.WordPrefixBonus (WordPrefixBonus)
import New.Engine.Metric.WordSuffixBonus (WordSuffixBonus)


spec :: Spec
spec = do
    describe "updateValue function" $ do
        it "map is updated" $ let
            node  = Tree.Node 0 mempty
            state = Match.mkState def
            updatedMap = runIdentity
                $! State.evalDefT @WordSuffixBonus
                .  State.evalDefT @WordPrefixBonus
                .  State.evalDefT @SuffixBonus
                .  State.evalDefT @SkipPenalty
                .  State.evalDefT @SequenceBonus
                .  State.evalDefT @PrefixBonus
                $! Search.updateValue node state mempty
            in updatedMap `shouldBe` Map.singleton 0 def
        it "match kind is correct" $ let
            node          = Tree.Node 0 mempty
            state         = Match.mkState def
            scoreMapM     = Search.updateValue node state mempty
            mayMatchM     = Map.lookup 0 <$> scoreMapM
            mayMatchKindM = view Match.kind `fmap2` mayMatchM
            updatedKind   = runIdentity
                $! State.evalDefT @WordSuffixBonus
                .  State.evalDefT @WordPrefixBonus
                .  State.evalDefT @SuffixBonus
                .  State.evalDefT @SkipPenalty
                .  State.evalDefT @SequenceBonus
                .  State.evalDefT @PrefixBonus
                $! mayMatchKindM
            in updatedKind `shouldBe` Just Substring.Equal
    describe "matchQuery function" $ do
        it "all values from tree are in map" $ let
            input :: [Text]
            input     = ["aa", "ab"]
            database  = Database.mk input
            root      = database ^. Database.tree
            hints'    = database ^. Database.hints
            resultMap = runIdentity
                $! State.evalDefT @WordSuffixBonus
                .  State.evalDefT @WordPrefixBonus
                .  State.evalDefT @SuffixBonus
                .  State.evalDefT @SkipPenalty
                .  State.evalDefT @SequenceBonus
                .  State.evalDefT @PrefixBonus
                $! Search.matchQuery mempty root
            in Map.keys hints' `shouldMatchList` Map.keys resultMap
        it "case sensitive is better than insensitive" $ let
            input :: [Text]
            input = ["bar", "Bar"]
            database = Database.mk input
            root     = database ^. Database.tree
            results  = runIdentity
                $! State.evalDefT @WordSuffixBonus
                .  State.evalDefT @WordPrefixBonus
                .  State.evalDefT @SuffixBonus
                .  State.evalDefT @SkipPenalty
                .  State.evalDefT @SequenceBonus
                .  State.evalDefT @PrefixBonus
                $! Search.matchQuery "bar" root
            maxIdx   = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 0
        it "equality is better then matching" $ let
            input :: [Text]
            input = ["baru", "Bar"]
            database = Database.mk input
            root     = database ^. Database.tree
            results  = runIdentity
                $! State.evalDefT @WordSuffixBonus
                .  State.evalDefT @WordPrefixBonus
                .  State.evalDefT @SuffixBonus
                .  State.evalDefT @SkipPenalty
                .  State.evalDefT @SequenceBonus
                .  State.evalDefT @PrefixBonus
                $! Search.matchQuery "bar" root
            maxIdx   = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 1
        it "matching all is better than not" $ let
            input :: [Text]
            input = ["abc", "adc"]
            database = Database.mk input
            root     = database ^. Database.tree
            results  = runIdentity
                $! State.evalDefT @WordSuffixBonus
                .  State.evalDefT @WordPrefixBonus
                .  State.evalDefT @SuffixBonus
                .  State.evalDefT @SkipPenalty
                .  State.evalDefT @SequenceBonus
                .  State.evalDefT @PrefixBonus
                $! Search.matchQuery "ab" root
            maxIdx   = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 0
