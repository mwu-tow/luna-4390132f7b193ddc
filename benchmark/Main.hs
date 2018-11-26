module Main where

import Criterion.Main
import Prologue       hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Tree        as Tree

import Data.Text             (Text)
import New.Engine.Data       (SearcherData)
import New.Engine.Data.Index (Index, IndexMap)
import System.Random         (mkStdGen, randomRs)


wordLengthRange :: (Int, Int)
wordLengthRange = (3, 30)

generateTextInput :: Int -> [Text]
generateTextInput inputLength = do
    let gen         = mkStdGen 13
        wordsLength = take inputLength $ randomRs wordLengthRange gen
    Text.pack . flip take (randomRs ('a', 'z') gen) <$> wordsLength

insert :: SearcherData a => [a] -> Tree.Node
insert = State.evalDef @IndexMap
    . State.evalDefT @Index
    . foldlM (flip Tree.insert) def

insertMultiple :: SearcherData a => [a] -> Tree.Node
insertMultiple = State.evalDef @IndexMap
    . State.evalDefT @Index
    . flip Tree.insertMultiple def

main :: IO ()
main = defaultMain . pure $ env (pure $ generateTextInput 100000) $ \ ~input
    -> bgroup "Insert functions"
        [ bench "insert"         $ nf insert         input
        , bench "insertMultiple" $ nf insertMultiple input
        ]
