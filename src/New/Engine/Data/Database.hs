{-# LANGUAGE Strict #-}
module New.Engine.Data.Database where

import Prologue hiding (Index)

import qualified New.Engine.Data.Tree as Tree
import qualified Data.Map.Strict as Map
import qualified Control.Monad.State.Layered as State

import New.Engine.Data.Index (Index (Index), IndexMap)
import Data.Map.Strict (Map)
import Control.Lens (Getter, to)
import Data.Text    (Text)



--------------------------
-- === SearcherData === --
--------------------------


-- === Definition === --

class Eq a => SearcherData a where
    text           :: Getter a Text
    calculateScore :: Int -> a -> Int

instance SearcherData Text where
    text               = to id
    calculateScore p _ = p


----------------------
-- === Database === --
----------------------


-- === Definition === --

data Database a = Database
    { _hints :: Map Index [a]
    , _tree  :: Tree.Root
    } deriving (Eq, Generic, Show)

makeLenses ''Database

instance Default (Database a) where def    = mempty
instance Mempty  (Database a) where mempty = Database mempty def
instance NFData a => NFData (Database a)

nextIndex :: Database a -> Index
nextIndex database = Index $! Map.size hints' where
    hints' = database ^. hints
{-# INLINE nextIndex #-}

mk :: SearcherData a => [a] -> Database a
mk input = Database hints' root where
    toTxt          = \h -> h ^. text
    txtInput       = toTxt <$> input
    mkTree         = Tree.mk txtInput
    (root, txtMap) = State.run @IndexMap mkTree mempty
    insertHint     = \acc hint -> let 
        txt = hint ^. text
        in case Map.lookup txt txtMap of
            Nothing  -> acc
            Just idx -> Map.insertWith (<>) idx [hint] acc
    hints' = foldl insertHint mempty input
{-# INLINE mk #-}

textMap :: SearcherData a => Getter (Database a) IndexMap
textMap = to $ \d -> let
    toTxt []     = Nothing
    toTxt (h:_)  = Just $! h ^. text
    idxToTxtMap  = Map.mapMaybe toTxt $! d ^. hints
    idxToTxtList = toList idxToTxtMap
    txtToIdxList = swap <$> idxToTxtList
    txtToIdxMap  = fromList txtToIdxList
    in txtToIdxMap
{-# INLINE textMap #-}
