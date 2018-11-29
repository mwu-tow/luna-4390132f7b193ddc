module Main where

import Criterion.Main
import Prologue       hiding (Index)

import qualified Data.Map              as Map
import qualified New.Engine.Data.Match as Match
import qualified New.Engine.Data.Tree  as Tree
import qualified New.Engine.Search     as Search

import Data.Map              (Map)
import Data.Text             (Text)
import New.Engine.Data.Index (Index (Index), IndexMap)
import New.Engine.Data.Match (Match, MatchKind)
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

randomHintNode :: Tree.Node
randomHintNode = node where (_, _, node) = randomHint
{-# NOINLINE randomHintNode #-}

mergeInput :: (Match, Match)
mergeInput = (match1, match2) where
    (positions1, positions2) = splitAt (maxWordLength `quot` 2) $ 
        take maxWordLength $ randomRs (0, maxWordLength) $ mkStdGen 31
    mkMatch = foldl (flip Match.addPosition) mempty
    match1  = mkMatch positions1
    match2  = mkMatch positions2
{-# NOINLINE mergeInput #-}

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

test_insertUpdateValue :: (Text, Tree.Node, Index, IndexMap) -> Tree.Node
test_insertUpdateValue (k, n, idx, idxMap)
    = Tree.evalWith idx idxMap $ Tree.updateValue k n
{-# NOINLINE test_insertUpdateValue #-}

test_search :: (Text, Tree.Node) -> Map Index Result
test_search (query, tree) = search query tree
{-# NOINLINE test_search #-}

test_searchUpdateValue :: (Text, Tree.Node, MatchKind, Match, Map Index Result) 
    -> Map Index Result
test_searchUpdateValue (suffix, node, matchKind, matched, resultMap) 
    = Search.updateValue suffix node matchKind matched resultMap
{-# NOINLINE test_searchUpdateValue #-}

test_matchMerge :: (Match, Match) -> Match
test_matchMerge (m1, m2) = Match.merge m1 m2
{-# NOINLINE test_matchMerge #-}


------------------------
-- === Benchmarks === --
------------------------

benchInsert :: [Benchmark]
benchInsert =
    [ envBench "update value"
        ( pure ("", treeInput, inputNextIndex, inputIndexMap))
        test_insertUpdateValue
    , envBench "insert" (pure textInput) test_insert
    ]
{-# INLINE benchInsert #-}

benchSearch :: [Benchmark]
benchSearch =
    [ envBench "update value"
        ( pure ("", randomHintNode, Match.AllCharsMatched, mempty, mempty))
        test_searchUpdateValue
    , envBench "merge"  (pure mergeInput)                  test_matchMerge
    , envBench "search" (pure (randomHintText, treeInput)) test_search
    ]
{-# INLINE benchSearch #-}

main :: IO ()
main = do
    defaultMain
        [ bgroup   "insert" benchInsert
        , bgroup   "search" benchSearch
        , envBench "lookup" (pure (randomHintText, treeInput)) test_lookup
        ]
