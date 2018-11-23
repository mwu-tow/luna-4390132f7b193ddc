{-# LANGUAGE Strict #-}
module New.Engine.Data.Tree where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.List                   as List
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text

import Control.Arrow   ((&&&))
import Control.Lens    ((?~))
import Data.Map        (Map)
import New.Engine.Data (SearcherData, text)


----------------
-- === ID === --
----------------

-- === Definition === --

newtype ID = ID Int deriving (Eq, Show)

notExists :: ID
notExists = ID $ -1

instance Default ID where def = notExists

-- === Utils === --

isInvalid :: ID -> Bool
isInvalid (ID i) = i < 0

getID :: State.Monad ID m => m ID
getID = State.modify @ID (\(ID i) -> let x = ID $ i + 1 in (x,x))
{-# INLINE getID #-}

-------------------
-- === IDMap === --
-------------------

-- === Definition === --

type IDMap = Map Text ID


------------------
-- === Node === --
------------------

-- === Definition === --

data Node = Node
    { _nodeId   :: {-# UNPACK #-} !ID
    , _branches :: !(Map Char Node)
    } deriving (Eq, Show)

makeLenses ''Node

instance Default Node where def = Node def mempty

-- === API === --

insert :: TreeContext m a => a -> Node -> m Node
insert sd n = insertKeyed (sd ^. text) sd n where
    insertKeyed :: TreeContext m a => Text -> a -> Node -> m Node
    insertKeyed k v node = case textHead k of
        Nothing -> updateValue v node
        Just c  -> insertAtChar c (Text.drop 1 k) v node
    updateValue :: TreeContext m a => a -> Node -> m Node
    updateValue v node = if not . isInvalid $ node ^. nodeId
        then pure node
        else getID >>= \newId -> do
            State.modify_ @IDMap $ Map.insert (v ^. text) newId
            pure $ node & nodeId .~ newId
    insertAtChar :: TreeContext m a => Char -> Text -> a -> Node -> m Node
    insertAtChar c k v node = 
        let update v = node & branches . at c ?~ v
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
                updated <- addToNode
                    v
                    $ fromJust def $ node ^. branches . at c
                pure $ Map.insert c updated acc
        newBranches <- foldlMWithKey processData mempty $ groupByFirstLetter sd
        pure $ node & branches .~ newBranches
    updateValue :: TreeContext m a => [a] -> Node -> m Node
    updateValue sd node = if (not . isInvalid $ node ^. nodeId) || null sd
        then pure node
        else getID >>= \newId -> do
            State.modify_ @IDMap $ Map.insert ((unsafeHead sd) ^. text) newId
            pure $ node & nodeId .~ newId

-- === Utils ==== --

type TreeContext m a = (SearcherData a, State.Monad ID m, State.Monad IDMap m)

textHead :: Text -> Maybe Char
textHead t = if Text.null t then Nothing else Just $ Text.head t

foldlMWithKey :: Monad m => (a -> k -> b -> m a) -> a -> Map k b -> m a
foldlMWithKey f init = Map.foldlWithKey
    (\acc k v -> acc >>= \a -> f a k v)
    (pure init)

groupByFirstLetter :: [(Text, a)] -> Map Char [(Text,a)]
groupByFirstLetter sd = foldl
    (\acc (k, v) -> Map.insertWith (<>) k (pure v) acc)
    mempty
    $ mapMaybe detachHead sd

keyed :: SearcherData a => [a] -> [(Text, a)]
keyed = List.sortOn fst . fmap (view text &&& id)

detachHead :: (Text, a) -> Maybe (Char, (Text, a))
detachHead (k, v) = (, (Text.drop 1 k, v)) <$> textHead k

