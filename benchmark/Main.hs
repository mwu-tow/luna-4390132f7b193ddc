module Main where

import Criterion.Main
import Prologue       hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Criterion.Types             as Options
import qualified Data.List                   as List
import qualified New.Engine.Data.Index       as Index
import qualified New.Engine.Data.Substring   as Substring
import qualified New.Engine.Data.Tree        as Tree
import qualified New.Engine.Search           as Search


import Data.Map.Strict           (Map)
import Data.Text                 (Text)
import New.Engine.Data.Index     (Index (Index), IndexMap)
import New.Engine.Data.Result    (Match)
import New.Engine.Data.Substring (Substring)
import New.Engine.Data.Tree      (Tree)
import System.Random             (Random (random, randomR), mkStdGen, randomRs)



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

instance Random Index where
    random g    = (Index randomInt, nextGen) where
        (randomInt, nextGen) = random g
    randomR (Index beg, Index end) g = (Index randomInt, nextGen) where
        (randomInt, nextGen) = randomR (beg, end) g

textInput :: [Text]
textInput = do
    let gen              = mkStdGen 13
        wordsLength      = take inputLength $ randomRs wordLengthRange gen
        infCharList      = randomRs ('a', 'z') gen
        addWord (result, charStream) len
            = splitAt len charStream & _1 %~ (:result)
    convert . fst $ foldl addWord (mempty, infCharList) wordsLength
{-# NOINLINE textInput #-}

treeInput :: Tree
treeInput = Tree.mk textInput
{-# NOINLINE treeInput #-}

indexMap :: IndexMap
indexMap = treeInput ^. Tree.indexMap
{-# NOINLINE indexMap #-}

inputRoot :: Tree.Node
inputRoot = treeInput ^. Tree.root
{-# NOINLINE inputRoot #-}


nextIndex :: Index
nextIndex = treeInput ^. Tree.nextIndex
{-# NOINLINE nextIndex #-}

randomIndex :: Index
randomIndex = fst $ randomR (0, nextIndex - 1) $ mkStdGen 23
{-# NOINLINE randomIndex #-}

randomHint :: Text
randomHint = fst hint where
    (Just hint) = List.find (\(_, idx) -> idx == randomIndex) $ toList indexMap
{-# NOINLINE randomHint #-}

randomHintNode :: Tree.Node
randomHintNode = node where (Just node) = Tree.lookup randomHint treeInput
{-# NOINLINE randomHintNode #-}

mergeInput :: (Substring, Substring)
mergeInput = (substr1, substr2) where
    (positions1, positions2) = splitAt (maxWordLength `quot` 2) $
        take maxWordLength $ randomRs (0, maxWordLength) $ mkStdGen 31
    mkSubstr = foldl (flip Substring.addPosition) mempty
    substr1  = mkSubstr positions1
    substr2  = mkSubstr positions2
{-# NOINLINE mergeInput #-}

-------------------
-- === Utils === --
-------------------

envBench :: (NFData a, NFData env)
    => String -> IO env -> (env -> a) -> Benchmark
envBench name pre fun = env pre $ \ ~input -> bench name $ nf fun input
{-# INLINE envBench #-}


-- === Test functions === --

test_mkTree :: [Text] -> Tree
test_mkTree txts = Tree.mk txts
{-# NOINLINE test_mkTree #-}

test_insertToNode :: (Text, Tree.Node, IndexMap) -> (Tree.Node, IndexMap)
test_insertToNode (txt, node, idxMap) = let
    insertToNode = Tree.insertToNode txt txt node
    in State.run @IndexMap insertToNode idxMap
{-# NOINLINE test_insertToNode #-}

test_insertUpdateValue :: (Text, Tree.Node, IndexMap) -> (Tree.Node, IndexMap)
test_insertUpdateValue (k, n, idxMap) = let
    updateVal = Tree.updateValue k n
    in State.run @IndexMap updateVal idxMap
{-# NOINLINE test_insertUpdateValue #-}


test_nextIndex :: IndexMap -> Index
test_nextIndex idxMap = State.eval @IndexMap Index.get idxMap
{-# NOINLINE test_nextIndex #-}

test_lookup :: (Text, Tree) -> Maybe Tree.Node
test_lookup (txt, tree) = Tree.lookup txt tree
{-# NOINLINE test_lookup #-}

test_lookupNode :: (Text, Tree.Node) -> Maybe Tree.Node
test_lookupNode (txt, node) = Tree.lookupNode txt node
{-# NOINLINE test_lookupNode #-}


test_substrMerge :: (Substring, Substring) -> Substring
test_substrMerge (s1, s2) = Substring.merge s1 s2
{-# NOINLINE test_substrMerge #-}

test_searchUpdateValue
    :: (Text, Tree.Node, Substring.Kind, Substring, Map Index Match)
    -> Map Index Match
test_searchUpdateValue (suffix, node, sKind, matched, resultMap)
    = Search.updateValue suffix node sKind matched resultMap
{-# NOINLINE test_searchUpdateValue #-}

test_matchQuery :: (Text, Tree) -> Map Index Match
test_matchQuery (query, tree) = Search.matchQuery query tree
{-# NOINLINE test_matchQuery #-}

------------------------
-- === Benchmarks === --
------------------------

benchTree :: [Benchmark]
benchTree = benchmarks where
    benchmarks =
        [ bgroup "insert" benchInsert
        , bgroup "lookup" benchLookup ]
    benchInsert =
        [ envBench "mk" (pure textInput) test_mkTree
        , envBench "insertToNode"
            (pure (randomHint, inputRoot, def))
            test_insertToNode
        , envBench "update value"
            ( pure ("", inputRoot, indexMap))
            test_insertUpdateValue
        , envBench "nextIndex"
            (pure indexMap)
            test_nextIndex
        ]
    benchLookup =
        [ envBench "lookup"
            (pure (randomHint, treeInput))
            test_lookup
        , envBench "lookupNode"
            (pure (randomHint, inputRoot))
            test_lookupNode
        ]
{-# INLINE benchTree #-}

benchSearch :: [Benchmark]
benchSearch =
    [ envBench "updateValue"
        ( pure ("", randomHintNode, Substring.FullMatch, mempty, mempty))
        test_searchUpdateValue
    , envBench "substrMerge" (pure mergeInput)              test_substrMerge
    , envBench "matchQuery"   (pure (randomHint, treeInput)) test_matchQuery
    ]
{-# INLINE benchSearch #-}

main :: IO ()
main = let
    cfg = defaultConfig { Options.resamples = 10000 }
    in defaultMainWith cfg
        [ bgroup   "tree"   benchTree
        , bgroup   "search" benchSearch
        ]
