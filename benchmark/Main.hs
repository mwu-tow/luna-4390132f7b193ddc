module Main where

import Criterion.Main
import Prologue

import qualified Data.Text as Text

import Data.Text     (Text)
import System.Random (mkStdGen, randomRs)


wordLengthLimit :: Int
wordLengthLimit = 40

generateTextInput :: Int -> [Text]
generateTextInput inputLength = do
    let gen         = mkStdGen 13
        wordsLength = take inputLength $ randomRs (1, wordLengthLimit) gen
    Text.pack . flip take (randomRs ('a', 'z') gen) <$> wordsLength

main :: IO ()
main = defaultMain []
