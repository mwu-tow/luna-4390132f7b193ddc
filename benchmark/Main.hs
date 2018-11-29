module Main where

import Criterion.Main
import Prologue       hiding (Index)

import qualified Data.Map              as Map
import qualified New.Engine.Data.Tree  as Tree

import Data.Map              (Map)
import Data.Text             (Text)
import New.Engine.Data.Index (Index (Index), IndexMap)
import New.Engine.Search     (Result, search)
import System.Random         (mkStdGen, randomR, randomRs)



-------------------
-- === Input === --
-------------------


-- === Config === --

inputLength :: Int
inputLength = 50000

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 30

wordLengthRange :: (Int, Int)
wordLengthRange = (minWordLength, maxWordLength)


-- === Generated === --

textInput :: [Text]
textInput = do
    let gen              = mkStdGen 13
        wordsLength      = take inputLength $ randomRs wordLengthRange gen
        infCharList      = randomRs ('a', 'z') gen
        addWord (result, charStream) len
            = splitAt len charStream & _1 %~ (:result)
    convert . fst $ foldl addWord (mempty, infCharList) wordsLength
{-# NOINLINE textInput #-}

treeInputWithContext :: (Tree.Node, Index, IndexMap)
treeInputWithContext = Tree.run $ Tree.insertMultiple textInput def
{-# NOINLINE treeInputWithContext #-}

treeInput :: Tree.Node
treeInput = tree where (tree, _, _) = treeInputWithContext
{-# NOINLINE treeInput #-}

inputNextIndex :: Index
inputNextIndex = idx where (_, idx, _) = treeInputWithContext
{-# NOINLINE inputNextIndex #-}

inputIndexMap :: IndexMap
inputIndexMap = idxMap where (_, _, idxMap) = treeInputWithContext
{-# NOINLINE inputIndexMap #-}

randomHint :: (Text, Index, Tree.Node)
randomHint = (k, idx, n) where
    (tree, (Index maxIdx), idxMap) = treeInputWithContext
    randomK  = fst $ randomR (0, maxIdx - 1) $ mkStdGen 23
    (k, idx) = Map.elemAt randomK idxMap
    (Just n) = Tree.lookup k tree
{-# NOINLINE randomHint #-}

randomHintText :: Text
randomHintText = txt where (txt, _, _) = randomHint
{-# NOINLINE randomHintText #-}


-------------------
-- === Utils === --
-------------------

envBench :: (NFData a, NFData env)
    => String -> IO env -> (env -> a) -> Benchmark
envBench name pre fun = env pre $ \ ~input -> bench name $ nf fun input
{-# INLINE envBench #-}


-- === Test functions === --

test_insert :: [Text] -> Tree.Node
test_insert txts = Tree.eval $ Tree.insertMultiple txts def
{-# NOINLINE test_insert #-}

test_lookup :: (Text, Tree.Node) -> Maybe Tree.Node
test_lookup (k, tree) = Tree.lookup k tree
{-# NOINLINE test_lookup #-}

test_search :: (Text, Tree.Node) -> Map Index Result
test_search (query, tree) = search query tree
{-# NOINLINE test_search #-}

test_updateValue :: (Text, Tree.Node, Index, IndexMap) -> Tree.Node
test_updateValue (k, n, idx, idxMap)
    = Tree.evalWith idx idxMap $ Tree.updateValue k n
{-# NOINLINE test_updateValue #-}



------------------------
-- === Benchmarks === --
------------------------

benchInsert :: [Benchmark]
benchInsert =
    [ envBench "update value"
        ( pure ("", treeInput, inputNextIndex, inputIndexMap))
        test_updateValue
    , envBench "insert" (pure textInput) test_insert
    ]
{-# INLINE benchInsert #-}


main :: IO ()
main = do
    defaultMain
        [ bgroup   "insert" benchInsert
        , envBench "lookup" (pure (randomHintText, treeInput)) $ test_lookup
        , envBench "search" (pure (randomHintText, treeInput)) $ test_search
        ]
