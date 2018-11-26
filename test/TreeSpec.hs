module TreeSpec (spec) where

import Prologue   hiding (Index)
import Test.Hspec

import qualified Control.Monad.State.Layered as State
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Tree        as Tree

import Control.Exception    (throw)
import Control.Lens         (makePrisms)
import Data.Set             (Set)
import New.Engine.Data.Tree (Index (Index), IndexMap, Node (Node), branches,
                             index)


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


recursiveCheckTreeStructure :: Text -> IndexMap -> Node -> IO ()
recursiveCheckTreeStructure matchedPrefix indexMap dict = check where
    accFunction acc k v = if Text.null k
        then acc
        else Map.insertWith
            Map.union
            (Text.head k)
            (Map.singleton (Text.drop 1 k) v)
            acc
    slicedMap = Map.foldlWithKey accFunction mempty indexMap
    currentIndex = fromJust def $ Map.lookup mempty indexMap
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

checkTreeStructure :: IndexMap -> Node -> IO ()
checkTreeStructure indexMap dict = catch check handleException where
    check = recursiveCheckTreeStructure mempty indexMap dict
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
        it "works on empty tree" $ checkTreeStructure mempty def
        it "works with single letter" $ checkTreeStructure
            (Map.singleton "a" 0)
            $ Node def $ Map.singleton 'a' $ Node 0 mempty
        it "works with branched data" $ checkTreeStructure
            (Map.fromList [("aa", 0), ("ab", 1)])
            $ Node def $ Map.singleton
                'a' $ Node def $ Map.fromList
                    [ ('a', Node 0 mempty)
                    , ('b', Node 1 mempty) ]
        it "throws exception when map empty and dict not empty" $ shouldThrow
            (recursiveCheckTreeStructure
                mempty
                mempty
                $ Node def $ Map.singleton 'a' $ Node 0 mempty)
            dictionaryStructureExceptionSelector
        it "throws exception when map not empty and dict empty" $ shouldThrow
            (recursiveCheckTreeStructure mempty (Map.singleton "a" 0) def)
            dictionaryStructureExceptionSelector
        it "throws exception when map does not match dict" $ shouldThrow
            (recursiveCheckTreeStructure
                mempty
                (Map.singleton "ab" 0)
                $ Node def $ Map.singleton
                'a' $ Node def $ Map.fromList
                    [ ('a', Node 0 mempty)
                    , ('b', Node 1 mempty) ])
            dictionaryStructureExceptionSelector
    describe "test insert function" $ do
        it "value is in map" $
            let perform = State.execDefT @IndexMap . State.evalDefT @Index
            in perform (Tree.insert (Text.pack "a") def) >>=
                shouldBe (Map.singleton (Text.pack "a") 0)
        it "value is in dictionary" $
            let perform = State.evalDefT @IndexMap . State.evalDefT @Index
            in perform (Tree.insert (Text.pack "a") def) >>=
                checkTreeStructure (Map.singleton (Text.pack "a") $ Index 0)
        it "values are in map" $
            let perform = State.execDefT @IndexMap . State.evalDefT @Index
                action  = Tree.insert (Text.pack "aa") def
                    >>= Tree.insert (Text.pack "ab")
            in perform action >>= shouldBe
                (Map.fromList [(Text.pack "aa", 0), (Text.pack "ab", 1)])
        it "values are in dictionary" $
            let perform = State.evalDefT @IndexMap . State.evalDefT @Index
                action  = Tree.insert (Text.pack "aa") def
                    >>= Tree.insert (Text.pack "ab")
            in perform action >>= checkTreeStructure (Map.fromList
                [ (Text.pack "aa", 0)
                , (Text.pack "ab", 1) ])
    describe "test insertMultiple function" $ do
        it "value is in map" $
            let perform = State.execDefT @IndexMap . State.evalDefT @Index
            in perform (Tree.insertMultiple [Text.pack "a"] def) >>=
                shouldBe (Map.singleton (Text.pack "a") 0)
        it "value is in dictionary" $
            let perform = State.evalDefT @IndexMap . State.evalDefT @Index
            in perform (Tree.insertMultiple [Text.pack "a"] def) >>=
                checkTreeStructure (Map.singleton (Text.pack "a") 0)
        it "values are in map" $
            let perform = State.execDefT @IndexMap . State.evalDefT @Index
                action  = Tree.insertMultiple
                    [Text.pack "aa", Text.pack "ab"]
                    def
            in perform action >>= shouldBe
                (Map.fromList [(Text.pack "aa", 0), (Text.pack "ab", 1)])
        it "values are in dictionary" $
            let perform = State.evalDefT @IndexMap . State.evalDefT @Index
                action  = Tree.insertMultiple
                    [Text.pack "aa", Text.pack "ab"]
                    def
            in perform action >>= checkTreeStructure (Map.fromList
                [ (Text.pack "aa", 0)
                , (Text.pack "ab", 1) ])
