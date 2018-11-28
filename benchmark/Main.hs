module Main where

import Criterion.Main
import Prologue       hiding (Index)

import qualified New.Engine.Data.Tree as Tree

import Data.Map              (Map)
import Data.Text             (Text)
import New.Engine.Data.Index (Index)
import New.Engine.Search     (Result, search)
import System.Random         (mkStdGen, randomR, randomRs)


wordLengthRange :: (Int, Int)
wordLengthRange = (3, 30)

generateTextInput :: Int -> [Text]
generateTextInput inputLength = do
    let gen              = mkStdGen 13
        wordsLength      = take inputLength $ randomRs wordLengthRange gen
        infCharList      = randomRs ('a', 'z') gen
        addWord (result, charStream) len
            = splitAt len charStream & _1 %~ (:result)
    convert . fst $ foldl addWord (mempty, infCharList) wordsLength

randomExistingQuery :: [Text] -> Text
randomExistingQuery database = database !! idx where
    gen = mkStdGen 17
    idx = fst $ randomR (0, length database - 1) gen

mkEnv :: (Text, Tree.Node)
mkEnv = (randomExistingQuery db, test_insert db) where
    db = generateTextInput 10000

test_insert :: [Text] -> Tree.Node
test_insert txts = Tree.eval $ Tree.insertMultiple txts def
{-# NOINLINE test_insert #-}

test_search :: (Text, Tree.Node) -> Map Index Result
test_search (query, tree) = search query tree
{-# NOINLINE test_search #-}


main :: IO ()
main = do
    defaultMain
        [ env (pure $ generateTextInput 10000) $ \ ~database
            -> bench "insert" $ nf test_insert database
        , env (pure mkEnv) $ \ ~input
            -> bench "search" $ nf test_search input
        ]
