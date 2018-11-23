module TreeSpec (spec) where

import Prologue
import Test.Hspec

import qualified Control.Monad.State.Layered as State
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Tree  as Tree

import Control.Lens (makePrisms)
import Control.Exception    (throw)
import Data.Map             (Map)
import Data.Set             (Set)
import New.Engine.Data      (SearcherData, text)
import New.Engine.Data.Tree (ID (ID), IDMap, Node (Node), branches, nodeId)


data TreeStructureExceptionType
    = IncorrectId       ID         ID
    | IncorrectBranches (Set Char) (Set Char)
    deriving (Eq, Generic, Show, Typeable)

data TreeStructureException = TreeStructureException
    { _dictionaryKey :: Text
    , _exceptionType :: TreeStructureExceptionType
    } deriving (Eq, Generic, Show, Typeable)

makeLenses ''TreeStructureException
makePrisms ''TreeStructureExceptionType

instance Exception TreeStructureException


recursiveCheckTreeStructure :: Text -> IDMap -> Node -> IO ()
recursiveCheckTreeStructure matchedPrefix idMap dict = check where
    accFunction acc k v = if Text.null k
        then acc
        else Map.insertWith
            Map.union
            (Text.head k)
            (Map.singleton (Text.drop 1 k) v)
            acc
    slicedMap = Map.foldlWithKey accFunction mempty idMap
    currentId = fromJust (ID $ -1) $ Map.lookup Text.empty idMap
    checkForException :: (Eq a, Show a, Typeable a)
        => (a -> a -> TreeStructureExceptionType) -> a -> a -> IO ()
    checkForException tpe a b = when (a /= b) $ throw
        $ TreeStructureException matchedPrefix $ tpe a b
    check   = do
        checkForException IncorrectId currentId $ dict ^. nodeId
        checkForException
            IncorrectBranches
            (Map.keysSet slicedMap)
            (Map.keysSet $ dict ^. branches)
        forM_ (toList slicedMap) $ \(c, newIdMap) -> withJust
            (dict ^. branches . at c)
            $ recursiveCheckTreeStructure (Text.snoc matchedPrefix c) newIdMap

checkTreeStructure :: IDMap -> Node -> IO ()
checkTreeStructure idMap dict = catch check handleException where
    check = recursiveCheckTreeStructure mempty idMap dict
    handleException :: TreeStructureException -> IO ()
    handleException e =
        let k = e ^. dictionaryKey
            expectEq :: (Eq a, Show a) => a -> a -> IO ()
            expectEq m d = (k, m) `shouldBe` (k, d)
        in case e ^. exceptionType of
            IncorrectBranches m d -> expectEq m d
            IncorrectId       m d -> expectEq m d


dictionaryStructureExceptionSelector :: Selector TreeStructureException
dictionaryStructureExceptionSelector = const True

spec :: Spec
spec = do
    describe "tree structure check tests" $ do
        it "works on empty tree" $ checkTreeStructure mempty def
        it "works with single letter" $ checkTreeStructure
            (Map.singleton "a" $ ID 0)
            $ Node (ID $ -1) $ Map.singleton 'a' $ Node (ID 0) mempty
        it "works with branched data" $ checkTreeStructure
            (Map.fromList [("aa", ID 0), ("ab", ID 1)])
            $ Node (ID $ -1) $ Map.singleton
                'a' $ Node (ID $ -1) $ Map.fromList
                    [ ('a', Node (ID 0) mempty)
                    , ('b', Node (ID 1) mempty) ]
        it "throws exception when map empty and dict not empty" $ shouldThrow
            (recursiveCheckTreeStructure
                mempty
                mempty
                $ Node (ID $ -1) $ Map.singleton 'a' $ Node (ID 0) mempty)
            dictionaryStructureExceptionSelector
        it "throws exception when map not empty and dict empty" $ shouldThrow
            (recursiveCheckTreeStructure mempty (Map.singleton "a" $ ID 0) def)
            dictionaryStructureExceptionSelector
        it "throws exception when map does not match dict" $ shouldThrow
            (recursiveCheckTreeStructure
                mempty
                (Map.singleton "ab" $ ID 0)
                $ Node (ID $ -1) $ Map.singleton
                'a' $ Node (ID $ -1) $ Map.fromList
                    [ ('a', Node (ID 0) mempty)
                    , ('b', Node (ID 1) mempty) ])
            dictionaryStructureExceptionSelector
    describe "test insert function" $ do
        it "value is in map" $
            let perform = State.execDefT @IDMap . State.evalDefT @ID
            in perform (Tree.insert (Text.pack "a") def) >>=
                shouldBe (Map.singleton (Text.pack "a") $ ID 0)
        it "value is in dictionary" $
            let perform = State.evalDefT @IDMap . State.evalDefT @ID
            in perform (Tree.insert (Text.pack "a") def) >>=
                checkTreeStructure (Map.singleton (Text.pack "a") $ ID 0)
        it "values are in map" $
            let perform = State.execDefT @IDMap . State.evalDefT @ID
                action  = Tree.insert (Text.pack "aa") def
                    >>= Tree.insert (Text.pack "ab")
            in perform action >>= shouldBe
                (Map.fromList [(Text.pack "aa", ID 0), (Text.pack "ab", ID 1)])
        it "values are in dictionary" $
            let perform = State.evalDefT @IDMap . State.evalDefT @ID
                action  = Tree.insert (Text.pack "aa") def
                    >>= Tree.insert (Text.pack "ab")
            in perform action >>= checkTreeStructure (Map.fromList 
                [ (Text.pack "aa", ID 0)
                , (Text.pack "ab", ID 1) ])
    describe "test insertMultiple function" $ do
        it "value is in map" $
            let perform = State.execDefT @IDMap . State.evalDefT @ID
            in perform (Tree.insertMultiple [Text.pack "a"] def) >>=
                shouldBe (Map.singleton (Text.pack "a") $ ID 0)
        it "value is in dictionary" $
            let perform = State.evalDefT @IDMap . State.evalDefT @ID
            in perform (Tree.insertMultiple [Text.pack "a"] def) >>=
                checkTreeStructure (Map.singleton (Text.pack "a") $ ID 0)
        it "values are in map" $
            let perform = State.execDefT @IDMap . State.evalDefT @ID
                action  = Tree.insertMultiple
                    [Text.pack "aa", Text.pack "ab"]
                    def
            in perform action >>= shouldBe
                (Map.fromList [(Text.pack "aa", ID 0), (Text.pack "ab", ID 1)])
        it "values are in dictionary" $
            let perform = State.evalDefT @IDMap . State.evalDefT @ID
                action  = Tree.insertMultiple
                    [Text.pack "aa", Text.pack "ab"]
                    def
            in perform action >>= checkTreeStructure (Map.fromList 
                [ (Text.pack "aa", ID 0)
                , (Text.pack "ab", ID 1) ])
