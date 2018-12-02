module New.Engine.SearchSpec (spec) where

import Prologue   hiding (Index)
import Test.Hspec

import qualified Data.List                as List
import qualified Data.Map.Strict          as Map
import qualified New.Engine.Data.Database as Database
import qualified New.Engine.Data.Tree     as Tree
import qualified New.Engine.Data.Match     as Match
import qualified New.Engine.Data.Substring     as Substring
import qualified New.Engine.Search        as Search

import New.Engine.Search (mkMatchState)


spec :: Spec
spec = do
    describe "updateValue function" $ do
        it "map is updated" $ let
            node  = Tree.Node 0 mempty
            state = mkMatchState def
            in shouldBe
                (Search.updateValue node state mempty)
                (Map.singleton 0 def)
        it "match kind is correct" $ let
            node     = Tree.Node 0 mempty
            state    = mkMatchState def
            scoreMap = Search.updateValue node state mempty
            mayMatch = Map.lookup 0 scoreMap
            mayMatchKind = view Match.kind <$> mayMatch
            in mayMatchKind `shouldBe` Just Substring.Equal
    describe "matchQuery function" $ do
        it "all values from tree are in map" $ let
            input :: [Text]
            input = ["aa", "ab"]
            database = Database.mk input
            root     = database ^. Database.tree
            hints'   = database ^. Database.hints
            in shouldMatchList
                (Map.keys hints')
                (Map.keys $ Search.matchQuery mempty root)
        it "case sensitive is better than insensitive" $ let
            input :: [Text]
            input = ["bar", "Bar"]
            database = Database.mk input
            root     = database ^. Database.tree
            results  = Search.matchQuery "bar" root
            maxIdx   = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 0
        it "equality is better then matching" $ let
            input :: [Text]
            input = ["baru", "Bar"]
            database = Database.mk input
            root     = database ^. Database.tree
            results  = Search.matchQuery "bar" root
            maxIdx   = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 1
        it "matching all is better than not" $ let
            input :: [Text]
            input = ["abc", "adc"]
            database = Database.mk input
            root     = database ^. Database.tree
            results  = Search.matchQuery "ab" root
            maxIdx   = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 0
