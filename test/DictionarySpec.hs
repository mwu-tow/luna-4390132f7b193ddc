module DictionarySpec (spec) where

import New.Engine.Prelude
import Test.Hspec

import qualified Data.Map  as Map
import qualified Data.Text as Text

import Control.Exception (throw)
import Data.Map                   (Map)
import Data.Set                   (Set)
import New.Engine.Data.Dictionary (Dictionary (Node), ID, IDMap, branches, nodeId)
import New.Engine.Data (SearcherData, text)

data DictionaryStructureExceptionType
    = IncorrectId       ID         ID
    | IncorrectBranches (Set Char) (Set Char)
    deriving (Eq, Generic, Show, Typeable)

data DictionaryStructureException = DictionaryStructureException
    { _dictionaryKey :: Text
    , _exceptionType :: DictionaryStructureExceptionType
    } deriving (Eq, Generic, Show, Typeable)

makeLenses ''DictionaryStructureException
makePrisms ''DictionaryStructureExceptionType

instance Exception DictionaryStructureException

recursiveCheckTreeStructure :: Text -> IDMap -> Dictionary -> IO ()
recursiveCheckTreeStructure matchedPrefix idMap dict = check where
    accFunction acc k v = if Text.null k
        then acc
        else Map.insertWith
            Map.union
            (Text.head k)
            (Map.singleton (Text.drop 1 k) v)
            acc
    slicedMap = Map.foldlWithKey accFunction mempty idMap
    currentId = fromJust (-1) $ Map.lookup Text.empty idMap
    checkForException :: (Eq a, Show a, Typeable a) 
        => (a -> a -> DictionaryStructureExceptionType) -> a -> a -> IO ()
    checkForException tpe a b = when (a /= b) $ throw 
        $ DictionaryStructureException matchedPrefix $ tpe a b
    check   = do
        checkForException IncorrectId currentId $ dict ^. nodeId
        checkForException 
            IncorrectBranches 
            (Map.keysSet slicedMap) 
            (Map.keysSet $ dict ^. branches)
        forM_ (toList slicedMap) $ \(c, newIdMap) -> withJust
            (dict ^. branches . at c)
            $ recursiveCheckTreeStructure (Text.snoc matchedPrefix c) newIdMap

checkTreeStructure :: IDMap -> Dictionary -> IO ()
checkTreeStructure idMap dict = catch check handleException where
    check = recursiveCheckTreeStructure mempty idMap dict
    handleException :: DictionaryStructureException -> IO ()
    handleException e = 
        let k = e ^. dictionaryKey
            expectEq :: (Eq a, Show a) => a -> a -> IO ()
            expectEq m d = (k, m) `shouldBe` (k, d)
        in case e ^. exceptionType of
            IncorrectBranches m d -> expectEq m d
            IncorrectId       m d -> expectEq m d


dictionaryStructureExceptionSelector :: Selector DictionaryStructureException
dictionaryStructureExceptionSelector = const True

spec :: Spec
spec = do
    describe "tree structure check tests" $ do
        it "works on empty tree" $ checkTreeStructure mempty def
        it "works with single letter" $ checkTreeStructure
            (Map.singleton "a" 0)
            $ Node (-1) $ Map.singleton 'a' $ Node 0 mempty
        it "works with branched data" $ checkTreeStructure
            (Map.fromList [("aa", 0), ("ab", 1)])
            $ Node (-1) $ Map.singleton
                'a' $ Node (-1) $ Map.fromList
                    [ ('a', Node 0 mempty)
                    , ('b', Node 1 mempty) ]
        it "throws exception when map empty and dict not empty" $ shouldThrow
            (recursiveCheckTreeStructure 
                mempty
                mempty
                $ Node (-1) $ Map.singleton 'a' $ Node 0 mempty)
            dictionaryStructureExceptionSelector
        it "throws exception when map not empty and dict empty" $ shouldThrow 
            (recursiveCheckTreeStructure mempty (Map.singleton "a" 0) def)
            dictionaryStructureExceptionSelector
        it "throws exception when map does not match dict" $ shouldThrow
            (recursiveCheckTreeStructure 
                mempty 
                (Map.singleton "ab" 0) 
                $ Node (-1) $ Map.singleton
                'a' $ Node (-1) $ Map.fromList
                    [ ('a', Node 0 mempty)
                    , ('b', Node 1 mempty) ])
            dictionaryStructureExceptionSelector

