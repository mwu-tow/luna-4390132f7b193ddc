{-# LANGUAGE Strict #-}
module New.Engine.Data.Tree where

import Prologue hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Data.List                   as List
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text

import Control.Arrow   ((&&&))
import Control.Lens    (to, (?~), _Just)
import Data.Map        (Map)
import New.Engine.Data (SearcherData, text)


-------------------
-- === Index === --
-------------------

-- === Definition === --

newtype Index = Index Int deriving (Eq, Generic, Num, Ord, Show)
makeClassy ''Index

notExists :: Index
notExists = -1

instance Default Index where def = notExists
instance NFData  Index

-- === Utils === --

isInvalid :: Index -> Bool
isInvalid = (< 0)

getIndex :: State.Monad Index m => m Index
getIndex = State.modify @Index (\prev -> let curr = prev + 1 in (curr,curr))
{-# INLINE getIndex #-}

----------------------
-- === IndexMap === --
----------------------

-- === Definition === --

type IndexMap = Map Text Index


------------------
-- === Node === --
------------------

-- === Definition === --

data Node = Node
    { __index   :: {-# UNPACK #-} !Index
    , _branches :: !(Map Char Node)
    } deriving (Eq, Generic, Show)

makeLenses ''Node

instance Default  Node where def   = Node def mempty
instance HasIndex Node where index = node_index
instance NFData   Node

-- === API === --

insert :: TreeContext m a => a -> Node -> m Node
insert sd n = insertKeyed (sd ^. text) sd n where
    insertKeyed :: TreeContext m a => Text -> a -> Node -> m Node
    insertKeyed k v node = case textHead k of
        Nothing -> updateValue v node
        Just c  -> insertAtChar c (Text.drop 1 k) v node
    updateValue :: TreeContext m a => a -> Node -> m Node
    updateValue v node = if not . isInvalid $ node ^. index
        then pure node
        else do
            newIndex <- getIndex
            State.modify_ @IndexMap $ Map.insert (v ^. text) newIndex
            pure $ node & index .~ newIndex
    insertAtChar :: TreeContext m a => Char -> Text -> a -> Node -> m Node
    insertAtChar c k v node =
        let update val = node & branches . at c ?~ val
        in update <$> insertKeyed k v (fromJust def $ node ^. branches . at c)

insertMultiple :: TreeContext m a => [a] -> Node -> m Node
insertMultiple input n = addToNode (keyed input) n where
    addToNode :: TreeContext m a => [(Text, a)] -> Node -> m Node
    addToNode keyedData node = do
        let (targets, rest) = List.partition (Text.null . fst) keyedData
        updated <- updateValue (snd <$> targets) node
        updateBranches rest updated
    updateBranches :: TreeContext m a => [(Text, a)] -> Node -> m Node
    updateBranches sd node = do
        let processData acc c v = do
                updated <- addToNode v
                    $ fromJust def $ node ^. branches . at c
                pure $ Map.insert c updated acc
        newBranches <- foldlMWithKey processData mempty $ groupByFirstLetter sd
        pure $ node & branches .~ newBranches
    updateValue :: TreeContext m a => [a] -> Node -> m Node
    updateValue sd node = if (not . isInvalid $ node ^. index) || null sd
        then pure node
        else do
            newIndex <- getIndex
            let withText    = for_ (sd ^? to head . _Just . text)
                updateMap t = State.modify_ @IndexMap $ Map.insert t newIndex
            withText updateMap
            pure $ node & index .~ newIndex

-- === Utils ==== --

type TreeContext m a =
    ( SearcherData a
    , State.Monad Index m
    , State.Monad IndexMap m )

textHead :: Text -> Maybe Char
textHead t = if Text.null t then Nothing else Just $ Text.head t
{-# INLINE textHead #-}

foldlMWithKey :: Monad m => (a -> k -> b -> m a) -> a -> Map k b -> m a
foldlMWithKey f initVal = Map.foldlWithKey
    (\acc k v -> acc >>= \a -> f a k v)
    (pure initVal)
{-# INLINE foldlMWithKey #-}

groupByFirstLetter :: [(Text, a)] -> Map Char [(Text,a)]
groupByFirstLetter sd = foldl
    (\acc (k, v) -> Map.insertWith (<>) k (pure v) acc)
    mempty
    $ mapMaybe detachHead sd
{-# INLINE groupByFirstLetter #-}

keyed :: SearcherData a => [a] -> [(Text, a)]
keyed = List.sortOn fst . fmap (view text &&& id)
{-# INLINE keyed #-}

detachHead :: (Text, a) -> Maybe (Char, (Text, a))
detachHead (k, v) = (, (Text.drop 1 k, v)) <$> textHead k
{-# INLINE detachHead #-}

