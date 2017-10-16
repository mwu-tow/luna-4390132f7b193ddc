{-# LANGUAGE OverloadedStrings #-}
module MainSpec (spec) where

import qualified Data.List                    as List
import           FuzzyText
import           Prologue
import           Test.Hspec


makeEntries :: [Text] -> [RawEntry]
makeEntries = map makeEntry where
    makeEntry func = RawEntry func Function 1

topResultNameShouldBe :: [Match] -> Text -> Expectation
topResultNameShouldBe []    _ = expectationFailure "Result is empty"
topResultNameShouldBe (h:_) r = h ^. name `shouldBe` r

topResultShouldHaveBestScore :: [Match] -> Expectation
topResultShouldHaveBestScore []      = expectationFailure "Result is empty"
topResultShouldHaveBestScore [_]     = def
topResultShouldHaveBestScore (h:s:_) = h ^. score `shouldSatisfy` (> s ^. score)

spec :: Spec
spec = do
    describe "scoring function" $ do
        it "match starting with capital is prefered over others" $ do
            let res = fuzzySearch "bar" $ makeEntries ["fooBar", "optbaru"]
            res `topResultNameShouldBe` "fooBar"
        it "match starting with capital have better score" $ do
            let res = fuzzySearch "bar" $ makeEntries ["fooBar", "optbaru"]
            topResultShouldHaveBestScore res
        it "subsequence should be prefered over substring" $ do
            let res = fuzzySearch "abc" $ makeEntries ["abcdef", "aebdcf"]
            res `topResultNameShouldBe` "abcdef"
        it "subsequence should have better score than substring" $ do
            let res = fuzzySearch "abc" $ makeEntries ["abcdef", "aebdcf"]
            topResultShouldHaveBestScore res
        it "subsequence at beggining of the word is prefered over other subsequences" $ do
            let res = fuzzySearch "ele" $ makeEntries ["getElement", "delement"]
            res `topResultNameShouldBe` "getElement"
        it "subsequence at beggining of the word should be scored better than other subsequences" $ do
            let res = fuzzySearch "ele" $ makeEntries ["getElement", "delement"]
            topResultShouldHaveBestScore res
        it "subsequence at beggining of the word is prefered over other subsequences" $ do
            let res = fuzzySearch "abcf" $ makeEntries ["abxcdefx", "aebdcfx"]
            res `topResultNameShouldBe` "abxcdefx"
        it "subsequence at beggining of the word should be scored better than other subsequences" $ do
            let res = fuzzySearch "abcf" $ makeEntries ["abxcdefx", "aebdcfx"]
            topResultShouldHaveBestScore res
        it "last letter match should be prefered when results are similar" $ do
            let res = fuzzySearch "xxxx" $ makeEntries ["xxaaxx", "xxbbxxb"]
            res `topResultNameShouldBe` "xxaaxx"
        it "last letter match should have better score when results are similar" $ do
            let res = fuzzySearch "xxxx" $ makeEntries ["xxaaxx", "xxbbxxb"]
            topResultShouldHaveBestScore res
        it "longer subsequence is favored" $ do
            let res = fuzzySearch "xxxxx" $ makeEntries ["xxaaxxax", "xbbxxxxb"]
            res `topResultNameShouldBe` "xbbxxxxb"
        it "longer subsequence have better score" $ do
            let res = fuzzySearch "xxxxx" $ makeEntries ["xxaaxxax", "xbbxxxxb"]
            topResultShouldHaveBestScore res
        it "result with less omitted letters is prefered" $ do
            let res = fuzzySearch "xxxxx" $ makeEntries ["xxaxxxa", "xxbbxxxb"]
            res `topResultNameShouldBe` "xxaxxxa"
        it "omitting letters results with worse score" $ do
            let res = fuzzySearch "xxxxx" $ makeEntries ["xxaxxxa", "xxbbxxxb"]
            topResultShouldHaveBestScore res
        it "exact match is prefered" $ do
            let res = fuzzySearch "foobar" $ makeEntries ["fooBar", "foobar"]
            res `topResultNameShouldBe` "foobar"
        it "exact match has better score" $ do
            let res = fuzzySearch "foobar" $ makeEntries ["fooBar", "foobar"]
            topResultShouldHaveBestScore res
          
    describe "matched letters" $ do
        it "match should be eager" $ do
            let res = fuzzySearch "x" $ makeEntries ["xx"]
            view match (List.head res) `shouldBe` [(0,1)]
        it "match should be eager" $ do
            let res = fuzzySearch "xx" $ makeEntries ["xxx"]
            view match (List.head res) `shouldBe` [(0,2)]
            