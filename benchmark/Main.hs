{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Criterion.Main
import Prologue       hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Criterion.Types             as Options
import qualified Data.Map.Strict             as Map
import qualified New.Engine.Data.Database    as Database
import qualified New.Engine.Data.Index       as Index
import qualified New.Engine.Data.Substring   as Substring
import qualified New.Engine.Data.Match       as Match
import qualified New.Engine.Data.Tree        as Tree
import qualified New.Engine.Search           as Search

import Data.Map.Strict           (Map)
import Data.Text                 (Text)
import New.Engine.Data.Database  (Database)
import New.Engine.Data.Index     (Index, IndexMap)
import New.Engine.Data.Match     (Match)
import New.Engine.Data.Substring (Substring)
import System.Random             (Random (randomR), mkStdGen, randomRs)



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

databaseInput :: Database Text
databaseInput = Database.mk textInput
{-# NOINLINE databaseInput #-}

hintsMap :: Map Index [Text]
hintsMap = databaseInput ^. Database.hints

textMap :: IndexMap
textMap = databaseInput ^. Database.textMap
{-# NOINLINE textMap #-}

inputRoot :: Tree.Node
inputRoot = databaseInput ^. Database.tree
{-# NOINLINE inputRoot #-}

nextIndex :: Index
nextIndex = Database.nextIndex databaseInput
{-# NOINLINE nextIndex #-}

randomIndex :: Index
randomIndex = fst $ randomR (0, nextIndex - 1) $ mkStdGen 23
{-# NOINLINE randomIndex #-}

randomHint :: Text
randomHint = txt ^. Database.text where
    (Just txts) = Map.lookup randomIndex hintsMap
    (Just txt)  = head txts
{-# NOINLINE randomHint #-}

randomHintNode :: Tree.Node
randomHintNode = node where (Just node) = Tree.lookup randomHint inputRoot
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

test_mkTree :: [Text] -> (Tree.Root, IndexMap)
test_mkTree txts = State.run @IndexMap mkTree mempty where
    mkTree = Tree.mk txts
{-# NOINLINE test_mkTree #-}

test_insertToNode :: (Text, Tree.Node, IndexMap) -> (Tree.Node, IndexMap)
test_insertToNode (txt, node, idxMap) = let
    insertToNode = Tree.insertToNode txt txt node
    in State.run @IndexMap insertToNode idxMap
{-# NOINLINE test_insertToNode #-}

test_insertUpdateValue :: (Text, Tree.Node, IndexMap) -> (Tree.Node, IndexMap)
test_insertUpdateValue (txt, n, txtMap) = let
    updateVal = Tree.updateValue txt n
    in State.run @IndexMap updateVal txtMap
{-# NOINLINE test_insertUpdateValue #-}

test_nextIndex :: IndexMap -> Index
test_nextIndex txtMap = State.eval @IndexMap Index.get txtMap
{-# NOINLINE test_nextIndex #-}

test_lookup :: (Text, Tree.Root) -> Maybe Tree.Node
test_lookup (txt, root) = Tree.lookup txt root
{-# NOINLINE test_lookup #-}

test_lookupNode :: (Text, Tree.Node) -> Maybe Tree.Node
test_lookupNode (txt, node) = Tree.lookupNode txt node
{-# NOINLINE test_lookupNode #-}

test_substrMerge :: (Substring, Substring) -> Substring
test_substrMerge (s1, s2) = Substring.merge s1 s2
{-# NOINLINE test_substrMerge #-}

test_searchUpdateValue
    :: (Tree.Node, Match.State, Map Index Match)
    -> Map Index Match
test_searchUpdateValue (node, state, resultMap)
    = Search.updateValue node state resultMap
{-# NOINLINE test_searchUpdateValue #-}

test_matchQuery :: (Text, Tree.Root) -> Map Index Match
test_matchQuery (query, root) = Search.matchQuery query root
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
            ( pure ("", inputRoot, textMap))
            test_insertUpdateValue
        , envBench "nextIndex"
            (pure textMap)
            test_nextIndex
        ]
    benchLookup =
        [ envBench "lookup"
            (pure (randomHint, inputRoot))
            test_lookup
        , envBench "lookupNode"
            (pure (randomHint, inputRoot))
            test_lookupNode
        ]
{-# INLINE benchTree #-}

benchSearch :: [Benchmark]
benchSearch =
    [ envBench "updateValue"
        ( pure (randomHintNode, Match.mkState def, mempty))
        test_searchUpdateValue
    , envBench "substrMerge" (pure mergeInput)              test_substrMerge
    , envBench "matchQuery"  (pure (randomHint, inputRoot)) test_matchQuery
    ]
{-# INLINE benchSearch #-}

main :: IO ()
main = let
    cfg = defaultConfig { Options.resamples = 10000 }
    in defaultMainWith cfg
        [ bgroup   "tree"   benchTree
        , bgroup   "search" benchSearch
        ]

