module Main where

import Criterion.Main
import Prologue       hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Tree        as Tree

import Data.Map             (Map)
import Data.Text             (Text)
import New.Engine.Data       (SearcherData)
import New.Engine.Data.Index (Index, IndexMap)
import New.Engine.Search (search, Result)
import System.Random         (mkStdGen, randomRs, randomR)


wordLengthRange :: (Int, Int)
wordLengthRange = (3, 30)

generateTextInput :: Int -> [Text]
generateTextInput inputLength = do
    let gen              = mkStdGen 13
        wordsLength      = take inputLength $ randomRs wordLengthRange gen
        infCharList      = randomRs ('a', 'z') gen
        addWord (words, charStream) len 
            = splitAt len charStream & _1 %~ (:words)
    convert . fst $ foldl addWord (mempty, infCharList) wordsLength


test_insert :: [Text] -> Tree.Node
test_insert txts = Tree.eval $ Tree.insertMultiple txts def
{-# NOINLINE test_insert #-}


main :: IO ()
main = do 
    defaultMain 
        [ env (pure $ generateTextInput 100000) $ \ ~database 
            -> bench "insert" $ nf test_insert database
        ]