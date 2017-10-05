{-# LANGUAGE OverloadedStrings #-}
module MainSpec (spec) where

import qualified Data.List                    as List
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           FuzzyText
import           LunaStudio.Data.NodeSearcher (ClassHints, ImportName, ModuleHints (ModuleHints))
import           Prologue
import           Test.Hspec


makeTestModule :: [Text] -> Map Text ClassHints -> Map ImportName ModuleHints
makeTestModule = Map.singleton "TestModule" .: ModuleHints

makeTestModuleEntries :: [Text] -> Map Text ClassHints -> Bool -> [Entry]
makeTestModuleEntries funs classes = toEntries (makeTestModule funs classes)

topResultNameShouldBe :: [Entry] -> Text -> Expectation
topResultNameShouldBe []    _ = expectationFailure "Result is empty"
topResultNameShouldBe (h:_) r = h ^. name `shouldBe` r

topResultShouldHaveBestScore :: [Entry] -> Expectation
topResultShouldHaveBestScore []      = expectationFailure "Result is empty"
topResultShouldHaveBestScore [_]     = def
topResultShouldHaveBestScore (h:s:_) = h ^. score `shouldSatisfy` (> s ^. score)

spec :: Spec
spec = do
    describe "scoring function" $ do
        it "match starting with capital is prefered over others" $ do
            let res = processEntries "bar" $ makeTestModuleEntries ["fooBar", "optbaru"] def False
            res `topResultNameShouldBe` "fooBar"
        it "match starting with capital have better score" $ do
            let res = processEntries "bar" $ makeTestModuleEntries ["fooBar", "optbaru"] def False
            topResultShouldHaveBestScore res
        it "subsequence should be prefered over substring" $ do
            let res = processEntries "abc" $ makeTestModuleEntries ["abcdef", "aebdcf"] def False
            res `topResultNameShouldBe` "abcdef"
        it "subsequence should have better score than substring" $ do
            let res = processEntries "abc" $ makeTestModuleEntries ["abcdef", "aebdcf"] def False
            topResultShouldHaveBestScore res
        it "subsequence at beggining of the word is prefered over other subsequences" $ do
            let res = processEntries "ele" $ makeTestModuleEntries ["getElement", "delement"] def False
            res `topResultNameShouldBe` "getElement"
        it "subsequence at beggining of the word should be scored better than other subsequences" $ do
            let res = processEntries "ele" $ makeTestModuleEntries ["getElement", "delement"] def False
            topResultShouldHaveBestScore res
        it "subsequence at beggining of the word is prefered over other subsequences" $ do
            let res = processEntries "abcf" $ makeTestModuleEntries ["abxcdefx", "aebdcfx"] def False
            res `topResultNameShouldBe` "abxcdefx"
        it "subsequence at beggining of the word should be scored better than other subsequences" $ do
            let res = processEntries "abcf" $ makeTestModuleEntries ["abxcdefx", "aebdcfx"] def False
            topResultShouldHaveBestScore res
        it "last letter match should be prefered when results are similar" $ do
            let res = processEntries "xxxx" $ makeTestModuleEntries ["xxaaxx", "xxbbxxb"] def False
            res `topResultNameShouldBe` "xxaaxx"
        it "last letter match should have better score when results are similar" $ do
            let res = processEntries "xxxx" $ makeTestModuleEntries ["xxaaxx", "xxbbxxb"] def False
            topResultShouldHaveBestScore res
        it "longer subsequence is favored" $ do
            let res = processEntries "xxxxx" $ makeTestModuleEntries ["xxaaxxax", "xbbxxxxb"] def False
            res `topResultNameShouldBe` "xbbxxxxb"
        it "longer subsequence have better score" $ do
            let res = processEntries "xxxxx" $ makeTestModuleEntries ["xxaaxxax", "xbbxxxxb"] def False
            topResultShouldHaveBestScore res
        it "result with less omitted letters is prefered" $ do
            let res = processEntries "xxxxx" $ makeTestModuleEntries ["xxaxxxa", "xxbbxxxb"] def False
            res `topResultNameShouldBe` "xxaxxxa"
        it "omitting letters results with worse score" $ do
            let res = processEntries "xxxxx" $ makeTestModuleEntries ["xxaxxxa", "xxbbxxxb"] def False
            topResultShouldHaveBestScore res
        it "exact match is prefered" $ do
            let res = processEntries "foobar" $ makeTestModuleEntries ["fooBar", "foobar"] def False
            res `topResultNameShouldBe` "foobar"
        it "exact match has better score" $ do
            let res = processEntries "foobar" $ makeTestModuleEntries ["fooBar", "foobar"] def False
            topResultShouldHaveBestScore res
          
    describe "matched letters" $ do
        it "match should be eager" $ do
            let res = processEntries "x" $ makeTestModuleEntries ["xx"] def False
            view match (List.head res) `shouldBe` [(0,0)]
        it "match should be eager" $ do
            let res = processEntries "xx" $ makeTestModuleEntries ["xxx"] def False
            view match (List.head res) `shouldBe` [(0,1)]
            