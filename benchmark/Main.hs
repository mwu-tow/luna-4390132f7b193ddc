module Main where

import Prologue hiding (Index)

import Criterion.Main

import qualified Control.Monad.State.Strict     as State
import qualified Criterion.Types                as Options
import qualified Data.Map.Strict                as Map
import qualified Searcher.Engine.Data.Database  as Database
import qualified Searcher.Engine.Data.Index     as Index
import qualified Searcher.Engine.Data.Match     as Match
import qualified Searcher.Engine.Data.Substring as Substring
import qualified Searcher.Engine.Data.Tree      as Tree
import qualified Searcher.Engine.Search         as Search

import Data.Map.Strict                        ( Map )
import Data.Text                              ( Text )
import Searcher.Engine.Data.Database          ( Database, SearcherData )
import Searcher.Engine.Data.Index             ( Index, IndexMap )
import Searcher.Engine.Data.Match             ( Match )
import Searcher.Engine.Data.Result            ( Result )
import Searcher.Engine.Data.Substring         ( Substring )
import Searcher.Engine.Metric.DefaultMetric   ( DefaultMetric )
import System.Random                          ( Random (randomR), mkStdGen
                                              , randomRs )



-------------------
-- === Input === --
-------------------

-- === Config === --

inputLength :: Int
inputLength = 5000

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 30

wordLengthRange :: (Int, Int)
wordLengthRange = (minWordLength, maxWordLength)


-- === Generated === --

textInput :: [Text]
textInput = do
    let gen         = mkStdGen 13
        wordsLength = take inputLength $ randomRs wordLengthRange gen
        infCharList = randomRs ('a', 'z') gen
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

defSearch :: SearcherData a => Text -> Database a -> [Result a]
defSearch = \query database ->
    Search.search query database (const 1) (def @DefaultMetric)
{-# INLINE defSearch #-}

defMatchQuery :: Text -> Tree.Root -> (Map Index Match)
defMatchQuery = \query database ->
    Search.matchQuery query database (def @DefaultMetric)
{-# INLINE defMatchQuery #-}

defSearchUpdateValue
    :: Tree.Node -> Match.State -> Map Index Match -> (Map Index Match)
defSearchUpdateValue = \node state resultMap ->
    Search.updateValue node state resultMap (def @DefaultMetric)
{-# INLINE defSearchUpdateValue #-}


-- === Test functions === --

test_mkTree :: [Text] -> (Tree.Root, IndexMap)
test_mkTree txts = State.runState @IndexMap mkTree mempty where
    mkTree = Tree.mk txts
{-# NOINLINE test_mkTree #-}

test_insertToNode :: (Text, Tree.Node, IndexMap) -> (Tree.Node, IndexMap)
test_insertToNode (txt, node, idxMap) = let
    insertToNode = Tree.insertToNode txt txt node
    in State.runState @IndexMap insertToNode idxMap
{-# NOINLINE test_insertToNode #-}

test_insertUpdateValue :: (Text, Tree.Node, IndexMap) -> (Tree.Node, IndexMap)
test_insertUpdateValue (txt, n, txtMap) = let
    updateVal = Tree.updateValue txt n
    in State.runState @IndexMap updateVal txtMap
{-# NOINLINE test_insertUpdateValue #-}

test_nextIndex :: IndexMap -> Index
test_nextIndex txtMap = State.evalState @IndexMap Index.get txtMap
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

test_searchUpdateValue :: (Tree.Node, Match.State, Map Index Match)
    -> Map Index Match
test_searchUpdateValue (node, state, resultMap)
    = defSearchUpdateValue node state resultMap
{-# NOINLINE test_searchUpdateValue #-}

test_matchQuery :: (Text, Tree.Root) -> Map Index Match
test_matchQuery (query, root) = defMatchQuery query root
{-# NOINLINE test_matchQuery #-}



------------------------
-- === Benchmarks === --
------------------------

main :: IO ()
main = let
    cfg = defaultConfig { Options.resamples = 100 }
    in defaultMainWith cfg
        [ bgroup   "tree"   benchTree
        , bgroup   "search" benchSearch
        ]

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

