{-# LANGUAGE OverloadedStrings #-}
module Searcher.Engine.MainSpec (spec) where

import Searcher.Engine.Prelude

import qualified Data.List                   as List
import qualified Searcher.Engine.Data        as Data
import qualified Searcher.Engine.Data.Match  as Match
import qualified Searcher.Engine.Data.Score  as Score

import Searcher.Engine
import Searcher.Engine.Data.Match  (Match)
import Searcher.Engine.Data.Symbol (Kind (Function, Method), Library (Library),
                                    Symbol (Symbol))
import Test.Hspec


makeSymbols :: [Text] -> [Symbol]
makeSymbols = map makeSymbol where
    makeSymbol func = Symbol func (Library def True) Function def 1 def

topResultNameShouldBe :: [Match Symbol] -> Text -> Expectation
topResultNameShouldBe []    _ = expectationFailure "Result is empty"
topResultNameShouldBe (h:_) r = (h ^. Data.name) `shouldBe` r

topResultEntryShouldBe :: [Match Symbol] -> Symbol -> Expectation
topResultEntryShouldBe [] _    = expectationFailure "Result is empty"
topResultEntryShouldBe (h:_) e = h ^. Match.hint `shouldBe` e

topResultShouldHaveBestScore :: [Match Symbol] -> Expectation
topResultShouldHaveBestScore []      = expectationFailure "Result is empty"
topResultShouldHaveBestScore [_]     = def
topResultShouldHaveBestScore (h:s:_) = h ^. Data.score . Score.total
    `shouldSatisfy` (> s ^. Data.score . Score.total)

spec :: Spec
spec = do
    describe "scoring function" $ do
        it "match starting with capital is prefered over others" $ do
            let res = search "bar" $ makeSymbols ["fooBar", "optbaru"]
            res `topResultNameShouldBe` "fooBar"
        it "match starting with capital have better score" $ do
            let res = search "bar" $ makeSymbols ["fooBar", "optbaru"]
            topResultShouldHaveBestScore res
        it "subsequence should be prefered over substring" $ do
            let res = search "abc" $ makeSymbols ["abcdef", "aebdcf"]
            res `topResultNameShouldBe` "abcdef"
        it "subsequence should have better score than substring" $ do
            let res = search "abc" $ makeSymbols ["abcdef", "aebdcf"]
            topResultShouldHaveBestScore res
        it "subsequence at beggining of the word is prefered over other subsequences" $ do
            let res = search "ele" $ makeSymbols ["getElement", "delement"]
            res `topResultNameShouldBe` "getElement"
        it "subsequence at beggining of the word should be scored better than other subsequences" $ do
            let res = search "ele" $ makeSymbols ["getElement", "delement"]
            topResultShouldHaveBestScore res
        it "subsequence at beggining of the word is prefered over other subsequences" $ do
            let res = search "abcf" $ makeSymbols ["abxcdefx", "aebdcfx"]
            res `topResultNameShouldBe` "abxcdefx"
        it "subsequence at beggining of the word should be scored better than other subsequences" $ do
            let res = search "abcf" $ makeSymbols ["abxcdefx", "aebdcfx"]
            topResultShouldHaveBestScore res
        it "last letter match should be prefered when results are similar" $ do
            let res = search "xxxx" $ makeSymbols ["xxaaxx", "xxbbxxb"]
            res `topResultNameShouldBe` "xxaaxx"
        it "last letter match should have better score when results are similar" $ do
            let res = search "xxxx" $ makeSymbols ["xxaaxx", "xxbbxxb"]
            topResultShouldHaveBestScore res
        it "longer subsequence is favored" $ do
            let res = search "xxxxx" $ makeSymbols ["xxaaxxax", "xbbxxxxb"]
            res `topResultNameShouldBe` "xbbxxxxb"
        it "longer subsequence have better score" $ do
            let res = search "xxxxx" $ makeSymbols ["xxaaxxax", "xbbxxxxb"]
            topResultShouldHaveBestScore res
        it "result with less omitted letters is prefered" $ do
            let res = search "xxxxx" $ makeSymbols ["xxaxxxa", "xxbbxxxb"]
            res `topResultNameShouldBe` "xxaxxxa"
        it "omitting letters results with worse score" $ do
            let res = search "xxxxx" $ makeSymbols ["xxaxxxa", "xxbbxxxb"]
            topResultShouldHaveBestScore res
        it "exact match is prefered" $ do
            let res = search "stream" $ makeSymbols ["streamFrom", "stream"]
            res `topResultNameShouldBe` "stream"
        it "exact match is prefered when case differs" $ do
            let res = search "foobar" $ makeSymbols ["fooBar", "foobar"]
            res `topResultNameShouldBe` "foobar"
        it "exact match has better score" $ do
            let res = search "foobar" $ makeSymbols ["fooBar", "foobar"]
            topResultShouldHaveBestScore res
        it "methods matching class are first for empty query" $ do
            let bestEntry = Symbol "%" (Library def True) (Method "Int")  def 0.7 def
                entries = [ Symbol "+" (Library def True) (Method "Int")  def 0.7 def
                          , Symbol "+" (Library def True) (Method "Text") def 0.5 def
                          , Symbol "%" (Library def True) (Method "Text") def 0.5 def
                          , Symbol "+" (Library def True) Function        def 0.3 def
                          , Symbol "%" (Library def True) Function        def 0.3 def
                          , Symbol "+" (Library def True) Function        def 0.2 def
                          , bestEntry
                          ]
                res = search "" entries
            res `topResultEntryShouldBe` bestEntry

    describe "matched letters" $ do
        it "match should be eager" $ do
            let res = search "x" $ makeSymbols ["xx"]
            view Match.matchedCharacters (List.head res) `shouldBe` [(0,1)]
        it "match should be eager" $ do
            let res = search "xx" $ makeSymbols ["xxx"]
            view Match.matchedCharacters (List.head res) `shouldBe` [(0,2)]

