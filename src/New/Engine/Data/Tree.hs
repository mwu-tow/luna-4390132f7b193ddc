{-# LANGUAGE Strict #-}

module New.Engine.Data.Tree where

import Prologue hiding (Index, lookup)

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Index       as Index

import Control.Lens          (to, (?~), _Just)
import Data.Map.Strict       (Map)
import New.Engine.Data.Index (Index, IndexMap)



------------------
-- === Node === --
------------------


-- === Definition === --

data Node = Node
    { _index   :: Index
    , _branches :: Map Char Node
    } deriving (Eq, Generic, Show)
makeLenses ''Node

instance Default  Node where def   = Node Index.notExists def
instance NFData   Node

type Root = Node


-- === API === --

mk :: State.Monad IndexMap m => [Text] -> m Root
mk = \txts -> insertMultiple txts def
{-# INLINE mk #-}

singleton :: State.Monad IndexMap m => Text -> m Root
singleton = \txt -> insert txt def
{-# INLINE singleton #-}

insert :: State.Monad IndexMap m => Text -> Root -> m Root
insert = \txt root -> insertToNode txt txt root
{-# INLINE insert #-}

insertToNode :: State.Monad IndexMap m => Text -> Text -> Node -> m Node
insertToNode = \suffix txt node -> case Text.uncons suffix of
    Nothing           -> updateValue txt node
    Just ((!h), (!t)) -> do
        let mayNextBranch = node ^. branches . at h
            nextBranch    = fromJust def mayNextBranch
        branch <- insertToNode t txt nextBranch
        pure $! node & branches . at h ?~ branch


updateValue :: State.Monad IndexMap m => Text -> Node -> m Node
updateValue = \txt node -> let
    nodeIdx = node ^. index
    updateMap = do
        newIndex <- Index.get
        State.modify_ @IndexMap $! Map.insert txt newIndex
        pure $! node & index .~ newIndex
    in if Index.isInvalid nodeIdx then updateMap else pure node
{-# INLINE updateValue #-}

insertMultiple :: State.Monad IndexMap m => [Text] -> Root -> m Root
insertMultiple = \txts root -> foldlM (flip insert) root txts
{-# INLINE insertMultiple #-}

lookup :: Text -> Root -> Maybe Node
lookup = \txt root -> lookupNode txt root
{-# INLINE lookup #-}

lookupNode :: Text -> Node -> Maybe Node
lookupNode = \txt n -> case Text.uncons txt of
    Nothing       -> Just n
    Just (!h, !t) -> n ^? branches . at h . _Just . to (lookupNode t) . _Just

