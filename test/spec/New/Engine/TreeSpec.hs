module New.Engine.TreeSpec (spec) where

import Prologue   hiding (Index)
import Test.Hspec

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Index       as Index
import qualified New.Engine.Data.Tree        as Tree

import Control.Exception     (throw)
import Control.Lens          (makePrisms)
import Data.Map.Strict       (Map)
import Data.Set              (Set)
import New.Engine.Data.Index (Index, IndexMap)
import New.Engine.Data.Tree  (Node (Node), Root, branches, index)


data TreeStructureExceptionType
    = IncorrectIndex    Index      Index
    | IncorrectBranches (Set Char) (Set Char)
    deriving (Eq, Generic, Show, Typeable)

data TreeStructureException = TreeStructureException
    { _dictionaryKey :: Text
    , _exceptionType :: TreeStructureExceptionType
    } deriving (Eq, Generic, Show, Typeable)

makeLenses ''TreeStructureException
makePrisms ''TreeStructureExceptionType

instance Exception TreeStructureException


recursiveCheckTreeStructure :: Text -> Map Text Index -> Node -> IO ()
recursiveCheckTreeStructure matchedPrefix indexMap dict = check where
    accFunction acc k v = if Text.null k
        then acc
        else Map.insertWith
            Map.union
            (Text.head k)
            (Map.singleton (Text.drop 1 k) v)
            acc
    slicedMap = Map.foldlWithKey accFunction mempty indexMap
    currentIndex = fromJust Index.notExists $ Map.lookup mempty indexMap
    checkForException :: (Eq a, Show a, Typeable a)
        => (a -> a -> TreeStructureExceptionType) -> a -> a -> IO ()
    checkForException tpe a b = when (a /= b) $ throw
        $ TreeStructureException matchedPrefix $ tpe a b
    check   = do
        checkForException IncorrectIndex currentIndex $ dict ^. index
        checkForException
            IncorrectBranches
            (Map.keysSet slicedMap)
            (Map.keysSet $ dict ^. branches)
        for_ (toList slicedMap) $ \(c, newIndexMap) ->
            for_ (dict ^. branches . at c) $ recursiveCheckTreeStructure
                (Text.snoc matchedPrefix c)
                newIndexMap

checkTreeStructure :: Root -> IndexMap -> IO ()
checkTreeStructure root indexMap = catch check handleException where
    check = recursiveCheckTreeStructure mempty indexMap root
    handleException :: TreeStructureException -> IO ()
    handleException e =
        let k = e ^. dictionaryKey
            expectEq :: (Eq a, Show a) => a -> a -> IO ()
            expectEq m d = (k, m) `shouldBe` (k, d)
        in case e ^. exceptionType of
            IncorrectBranches m d -> expectEq m d
            IncorrectIndex    m d -> expectEq m d


dictionaryStructureExceptionSelector :: Selector TreeStructureException
dictionaryStructureExceptionSelector = const True

spec :: Spec
spec = do
    describe "tree structure check tests" $ do
        it "works on empty tree" $ checkTreeStructure def def
        it "works with single letter" $ checkTreeStructure
            (Node Index.notExists $ Map.singleton 'a' $ Node 0 mempty)
            (Map.singleton "a" 0)
        it "works with branched data" $ flip checkTreeStructure
            (fromList [("aa", 0), ("ab", 1)])
            $ Node Index.notExists $ Map.singleton
                'a' $ Node Index.notExists $ fromList
                    [ ('a', Node 0 mempty)
                    , ('b', Node 1 mempty) ]
        it "throws exception when map empty and dict not empty" $ shouldThrow
            (recursiveCheckTreeStructure
                mempty
                mempty
                $ Node Index.notExists $ Map.singleton 'a' $ Node 0 mempty)
            dictionaryStructureExceptionSelector
        it "throws exception when map not empty and dict empty" $ shouldThrow
            (recursiveCheckTreeStructure mempty (Map.singleton "a" 0) def)
            dictionaryStructureExceptionSelector
        it "throws exception when map does not match dict" $ shouldThrow
            (recursiveCheckTreeStructure
                mempty
                (Map.singleton "ab" 0)
                $ Node Index.notExists $ Map.singleton
                'a' $ Node Index.notExists $ fromList
                    [ ('a', Node 0 mempty)
                    , ('b', Node 1 mempty) ])
            dictionaryStructureExceptionSelector
    describe "test insert function" $ do
        it "value is in map" $ let
            idxMap = State.exec @IndexMap (Tree.singleton "a") mempty
            in idxMap `shouldBe` Map.singleton "a" 0
        it "value is in dictionary" $ let
            (root, idxMap) = State.run @IndexMap (Tree.singleton "a") mempty
            in checkTreeStructure root idxMap
        it "values are in map" $ let
            idxMap = State.exec @IndexMap (Tree.mk ["aa", "ab"]) mempty
            in idxMap `shouldBe` fromList [("aa", 0), ("ab", 1)]
        it "values are in dictionary" $ let
            (root, idxMap) = State.run @IndexMap (Tree.mk ["aa", "ab"]) mempty
            in checkTreeStructure root idxMap

