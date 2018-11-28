module SearchSpec (spec) where

import Test.Hspec
import Prologue hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Tree        as Tree
import qualified Data.List as List
import qualified Data.Map as Map

import Data.Map (Map)
import New.Engine.Data.Index (IndexMap, Index)
import New.Engine.Search (search, Result)



spec :: Spec
spec = do
    describe "search function" $ do
        it "all values from tree are in map" $ let
            (tree, _, idxMap) = Tree.run $ Tree.insertMultiple ["aa", "ab"] def
            in shouldMatchList
                (Map.elems idxMap)
                (Map.keys $ search mempty tree)
        it "case sensitive is better than insensitive" $ let
            (tree, _, idxMap) = Tree.run 
                $ Tree.insertMultiple ["bar", "Bar"] def
            results = search "bar" tree 
            maxIdx  = fst $ List.maximumBy 
                (\el1 el2 -> snd el1 `compare` snd el2) 
                $ Map.toList results 
            in Just maxIdx `shouldBe` Map.lookup "bar" idxMap
        it "equality is better then matching" $ let
            (tree, _, idxMap) = Tree.run 
                $ Tree.insertMultiple ["baru", "Bar"] def
            results = search "bar" tree 
            maxIdx  = fst $ List.maximumBy 
                (\el1 el2 -> snd el1 `compare` snd el2) 
                $ Map.toList results 
            in Just maxIdx `shouldBe` Map.lookup "Bar" idxMap
        it "matching all is better than not" $ let
            (tree, _, idxMap) = Tree.run 
                $ Tree.insertMultiple ["abc", "adc"] def
            results = search "ab" tree 
            maxIdx  = fst $ List.maximumBy 
                (\el1 el2 -> snd el1 `compare` snd el2) 
                $ Map.toList results 
            in Just maxIdx `shouldBe` Map.lookup "abc" idxMap
