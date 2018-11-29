{-# LANGUAGE Strict #-}
module New.Engine.Data.Tree
    ( module New.Engine.Data.Tree
    , module X
    ) where

import New.Engine.Data.Index as X (HasIndex (index))

import Prologue hiding (Index, lookup)

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as Text
import qualified New.Engine.Data.Index       as Index

import Control.Lens          ((?~), to, _Just)
import Data.Map.Strict       (Map)
import New.Engine.Data.Index (Index, IndexMap)



------------------
-- === Node === --
------------------

-- === Definition === --

data Node = Node
    { __index   :: Index
    , _branches :: Map Char Node
    } deriving (Eq, Generic, Show)
makeLenses ''Node

instance Default  Node where def   = Node def def
instance HasIndex Node where index = node_index
instance NFData   Node


-- === API === --

type TreeContext m =
    ( State.Monad Index m
    , State.Monad IndexMap m 
    )

eval :: State.StateT Index (State.State IndexMap) a -> a
eval = State.evalDef @IndexMap
    . State.evalDefT @Index
{-# INLINE eval #-}

evalWith 
    :: Index -> IndexMap -> State.StateT Index (State.State IndexMap) a -> a
evalWith idx idxMap 
    = flip (State.eval  @IndexMap) idxMap 
    . flip (State.evalT @Index)    idx
{-# INLINE evalWith #-}

run :: State.StateT Index (State.State IndexMap) a -> (a, Index, IndexMap)
run = flatTuple . State.runDef @IndexMap . State.runDefT @Index
    where flatTuple ((a, b), c) = (a,b,c)
{-# INLINE run #-}

runWith :: Index -> IndexMap -> State.StateT Index (State.State IndexMap) a 
    -> (a, Index, IndexMap)
runWith idx idxMap = let flatTuple ((a, b), c) = (a,b,c) in flatTuple 
    . flip (State.run  @IndexMap) idxMap
    . flip (State.runT @Index)    idx
{-# INLINE runWith #-}

insert :: TreeContext m => Text -> Node -> m Node
insert = \txt n -> let
    insertKeyed :: TreeContext m => Text -> Node -> m Node
    insertKeyed k node = case Text.uncons k of
        Nothing          -> updateValue txt node
        Just (!c, !txt') -> insertAtChar c txt' node
    insertAtChar :: TreeContext m => Char -> Text -> Node -> m Node
    insertAtChar c k node =
        let update      = \val -> node & branches . at c ?~ val
            prevBranchM = node ^. branches . at c
            prevBranch  = fromJust def prevBranchM
            newBranch   = insertKeyed k prevBranch 
        in update <$> newBranch
    in insertKeyed txt n
{-# INLINE insert #-}

updateValue :: TreeContext m => Text -> Node -> m Node
updateValue k node = let 
    idx       = node ^. index
    updateMap = do 
        newIndex <- Index.get
        State.modify_ @IndexMap $! Map.insert k newIndex
        pure $! node & index .~ newIndex
    in if Index.isInvalid idx then updateMap else pure node
{-# INLINE updateValue #-}

insertMultiple :: TreeContext m => [Text] -> Node -> m Node
insertMultiple txts node = foldlM (flip insert) node txts
{-# INLINE insertMultiple #-}

lookup :: Text -> Node -> Maybe Node
lookup txt n = case Text.uncons txt of
    Nothing       -> Just n
    Just (!h, !t) -> n ^? branches . at h . _Just . to (lookup t) . _Just