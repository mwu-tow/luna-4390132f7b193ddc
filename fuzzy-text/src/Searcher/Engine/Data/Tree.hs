{-# LANGUAGE Strict #-}

module Searcher.Engine.Data.Tree where

import Prologue hiding (Index, lookup)

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as Text
import qualified Searcher.Engine.Data.Index as Index

import Control.Lens          (to, (?~), _Just)
import Data.Map.Strict       (Map)
import Searcher.Engine.Data.Index (Index, IndexMap)



------------------
-- === Node === --
------------------


-- === Definition === --

data Node = Node
    { _index    :: Index
    , _branches :: Map Char Node
    } deriving (Eq, Generic, Show)
makeLenses ''Node

type Root = Node


-- === API === --

mk :: State.MonadState IndexMap m => [Text] -> m Root
mk = \txts -> insertMultiple txts def
{-# INLINE mk #-}

singleton :: State.MonadState IndexMap m => Text -> m Root
singleton = \txt -> insert txt def
{-# INLINE singleton #-}

insert :: State.MonadState IndexMap m => Text -> Root -> m Root
insert = \txt root -> insertToNode txt txt root
{-# INLINE insert #-}

insertToNode :: State.MonadState IndexMap m => Text -> Text -> Node -> m Node
insertToNode = \suffix txt node -> case Text.uncons suffix of
    Nothing           -> updateValue txt node
    Just ((!h), (!t)) -> do
        let mayNextBranch = node ^. branches . at h
            nextBranch    = fromJust def mayNextBranch
        branch <- insertToNode t txt nextBranch
        pure $! node & branches . at h ?~ branch
{-# INLINEABLE insertToNode #-}

updateValue :: State.MonadState IndexMap m => Text -> Node -> m Node
updateValue = \txt node -> let
    nodeIdx = node ^. index
    updateMap = do
        newIndex <- Index.get
        State.modify' @IndexMap $! Map.insert txt newIndex
        pure $! node & index .~ newIndex
    in if Index.isInvalid nodeIdx then updateMap else pure node
{-# INLINEABLE updateValue #-}

insertMultiple :: State.MonadState IndexMap m => [Text] -> Root -> m Root
insertMultiple = \txts root -> foldlM (flip insert) root txts
{-# INLINE insertMultiple #-}

lookup :: Text -> Root -> Maybe Node
lookup = \txt root -> lookupNode txt root
{-# INLINE lookup #-}

lookupNode :: Text -> Node -> Maybe Node
lookupNode = \txt n -> case Text.uncons txt of
    Nothing       -> Just n
    Just (!h, !t) -> n ^? branches . at h . _Just . to (lookupNode t) . _Just


-- === Instances === --

instance Default  Node where def   = Node Index.notExists def
instance NFData   Node

