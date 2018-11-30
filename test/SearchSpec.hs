module SearchSpec (spec) where

import Prologue   hiding (Index)
import Test.Hspec

import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified New.Engine.Data.Tree as Tree
import qualified New.Engine.Search as Search



spec :: Spec
spec = do
    describe "matchQuery function" $ do
        it "all values from tree are in map" $ let
            tree = Tree.mk ["aa", "ab"]
            idxMap  = tree ^. Tree.indexMap
            in shouldMatchList
                (Map.elems idxMap)
                (Map.keys $ Search.matchQuery mempty tree)
        it "case sensitive is better than insensitive" $ let
            tree    = Tree.mk ["bar", "Bar"]
            idxMap  = tree ^. Tree.indexMap
            results = Search.matchQuery "bar" tree
            maxIdx  = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in Just maxIdx `shouldBe` Map.lookup "bar" idxMap
        it "equality is better then matching" $ let
            tree    = Tree.mk ["baru", "Bar"]
            idxMap  = tree ^. Tree.indexMap
            results = Search.matchQuery "bar" tree
            maxIdx  = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in Just maxIdx `shouldBe` Map.lookup "Bar" idxMap
        it "matching all is better than not" $ let
            tree    = Tree.mk ["abc", "adc"]
            idxMap  = tree ^. Tree.indexMap
            results = Search.matchQuery "ab" tree
            maxIdx  = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in Just maxIdx `shouldBe` Map.lookup "abc" idxMap
