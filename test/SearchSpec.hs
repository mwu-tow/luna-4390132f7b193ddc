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
            txtMap  = tree ^. Tree.textMap
            in shouldMatchList
                (Map.keys txtMap)
                (Map.keys $ Search.matchQuery mempty tree)
        it "case sensitive is better than insensitive" $ let
            tree    = Tree.mk ["bar", "Bar"]
            txtMap  = tree ^. Tree.textMap
            results = Search.matchQuery "bar" tree
            maxIdx  = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 0
        it "equality is better then matching" $ let
            tree    = Tree.mk ["baru", "Bar"]
            txtMap  = tree ^. Tree.textMap
            results = Search.matchQuery "bar" tree
            maxIdx  = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 1
        it "matching all is better than not" $ let
            tree    = Tree.mk ["abc", "adc"]
            txtMap  = tree ^. Tree.textMap
            results = Search.matchQuery "ab" tree
            maxIdx  = fst $ List.maximumBy
                (\el1 el2 -> snd el1 `compare` snd el2)
                $ Map.toList results
            in maxIdx `shouldBe` 0
