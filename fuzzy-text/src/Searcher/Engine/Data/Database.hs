{-# LANGUAGE Strict #-}

module Searcher.Engine.Data.Database where

import Prologue hiding (Index)

-- TODO [LSR]
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict            as Map
import qualified Searcher.Engine.Data.Tree  as Tree

import Control.Lens               (Getter, to)
import Data.Map.Strict            (Map)
import Data.Text                  (Text)
import Searcher.Engine.Data.Index (Index, IndexMap)
import Searcher.Engine.Data.Score (Score (Score))



--------------------------
-- === SearcherData === --
--------------------------

-- === Definition === --

class Eq a => SearcherData a where
    text           :: Getter a Text
    fixedScore     :: Getter a Score
    calculateScore :: Score -> (a -> Double) -> a -> Double
    calculateScore (Score points) weightGetter searcherData = let
        weight         = weightGetter searcherData
        (Score fixed)  = searcherData ^. fixedScore
        in (fromIntegral points) * weight + (fromIntegral fixed)
    {-# MINIMAL text, fixedScore #-}

instance SearcherData Text where
    text       = to id
    fixedScore = to $! const def


----------------------
-- === Database === --
----------------------

-- === Definition === --

data Database a = Database
    { _hints            :: Map Index [a]
    , _tree             :: Tree.Root
    } deriving (Eq, Generic, Show)

makeLenses ''Database

instance Default (Database a) where def    = mempty
instance Mempty  (Database a) where mempty = Database mempty def
instance NFData a => NFData (Database a)

nextIndex :: Database a -> Index
nextIndex database = Map.size hints' where
    hints' = database ^. hints
{-# INLINE nextIndex #-}

mk :: SearcherData a => [a] -> Database a
mk input = Database hints' root where
    toTxt          = \h -> h ^. text
    txtInput       = toTxt <$> input
    mkTree         = Tree.mk txtInput
    (root, txtMap) = State.runState @IndexMap mkTree mempty
    insertHint     = \acc hint -> let
        txt = hint ^. text
        in case Map.lookup txt txtMap of
            Nothing  -> acc
            Just idx -> Map.insertWith (<>) idx [hint] acc
    hints' = foldl' insertHint mempty input
{-# INLINE mk #-}

textMap :: SearcherData a => Getter (Database a) IndexMap
textMap = to $ \d -> let
    toTxt []    = Nothing
    toTxt (h:_) = Just $! h ^. text
    idxToTxtMap  = Map.mapMaybe toTxt $! d ^. hints
    idxToTxtList = toList idxToTxtMap
    txtToIdxList = swap <$> idxToTxtList
    txtToIdxMap  = fromList txtToIdxList
    in txtToIdxMap
{-# INLINE textMap #-}

insert :: SearcherData a => a -> Database a -> Database a
insert hint database = let
    txtMap  = database ^. textMap
    hintTxt = hint ^. text
    root    = database ^. tree
    insert' = Tree.insert hintTxt root
    (root', txtMap') = State.runState @IndexMap insert' txtMap
    mayIdx = Map.lookup hintTxt txtMap'
    updateHints = \idx hintMap -> Map.insertWith (<>) idx [hint] hintMap
    in database
        & hints %~ maybe id updateHints mayIdx
        & tree  .~ root'
{-# INLINE insert #-}

insertMultiple :: SearcherData a => [a] -> Database a -> Database a
insertMultiple input database = foldl (flip insert) database input
{-# INLINE insertMultiple #-}

