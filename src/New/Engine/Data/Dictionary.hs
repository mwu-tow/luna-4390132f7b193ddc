{-# LANGUAGE Strict #-}
module New.Engine.Data.Dictionary where

import New.Engine.Prelude

import qualified Control.Monad.State.Layered as State
import qualified Data.Map                    as Map
import qualified Data.Text                   as Text
import qualified Data.List                   as List

import Data.Map        (Map)
import New.Engine.Data (SearcherData, text)

type ID = Int
type IDMap = Map Text ID

data Dictionary = Node 
    { _nodeId   :: {-# UNPACK #-} !ID
    , _branches :: !(Map Char Dictionary)
    } deriving (Show)

makeLenses ''Dictionary

instance Default Dictionary where def = Node (-1) mempty


getID :: State.Monad ID m => m ID
getID = do
    currentId <- State.get @ID
    State.put @ID $ succ currentId
    pure currentId

insert :: (SearcherData a, State.Monad IDMap m, State.Monad ID m)
    => a -> Dictionary -> m Dictionary
insert sd = insertKeyed (sd ^. text, sd) where
    insertKeyed :: (SearcherData a, State.Monad IDMap m, State.Monad ID m) 
        => (Text, a) -> Dictionary -> m Dictionary
    insertKeyed (k, v) dict = if Text.null k
        then updateValue v dict
        else insertAtChar (Text.head k) (Text.drop 1 k, v) dict
    updateValue :: (SearcherData a, State.Monad IDMap m, State.Monad ID m)
        => a -> Dictionary -> m Dictionary
    updateValue v dict = if dict ^. nodeId >= 0
        then pure dict
        else getID >>= \newId -> do 
            State.modify_ @IDMap $ Map.insert (v ^. text) newId
            pure $ dict & nodeId .~ newId
    insertAtChar :: (SearcherData a, State.Monad IDMap m, State.Monad ID m)
        => Char -> (Text,a) -> Dictionary -> m Dictionary
    insertAtChar c v dict = do
        updatedBranch <- insertKeyed v $ fromJust def $ dict ^. branches . at c
        pure $ dict & branches . at c ?~ updatedBranch


insertMultiple :: (SearcherData a, State.Monad IDMap m, State.Monad ID m) 
    => [a] -> Dictionary -> m Dictionary
insertMultiple = addToDictionary . preprocess where
    preprocess :: SearcherData a => [a] -> [(Text, a)]
    preprocess = List.sortOn fst . fmap (view text &&& id)
    detachHead :: (Text, a) -> Maybe (Char, (Text, a))
    detachHead (k, v) = (, (Text.drop 1 k, v)) <$> textHead k
    group :: [(Text, a)] -> Map Char [(Text,a)]
    group sd = foldl
        (\acc (k, v) -> Map.insertWith (<>) k (pure v) acc)
        mempty
        $ mapMaybe detachHead sd    
    addToDictionary :: (SearcherData a, State.Monad IDMap m, State.Monad ID m) 
        => [(Text, a)] -> Dictionary -> m Dictionary
    addToDictionary keyedData dict = do
        let (targets, rest) = List.partition (Text.null . fst) keyedData
        updated <- updateValue (snd <$> targets) dict
        updateBranches rest updated
    foldlMWithKey :: Monad m => (a -> k -> b -> m a) -> a -> Map k b -> m a
    foldlMWithKey f init = Map.foldlWithKey 
        (\acc k v -> acc >>= \a -> f a k v) 
        (pure init)
    updateBranches :: (SearcherData a, State.Monad IDMap m, State.Monad ID m)
        => [(Text, a)] -> Dictionary -> m Dictionary
    updateBranches sd dict = do
        let processData acc c v = do
                updated <- addToDictionary 
                    v 
                    $ fromJust def $ dict ^. branches . at c
                pure $ Map.insert c updated acc
        newBranches <- foldlMWithKey processData mempty $ group sd
        pure $ dict & branches .~ newBranches
    updateValue :: (SearcherData a, State.Monad IDMap m, State.Monad ID m)
        => [a] -> Dictionary -> m Dictionary
    updateValue sd dict = if dict ^. nodeId >= 0 || null sd
        then pure dict
        else getID >>= \newId -> do 
            State.modify_ @IDMap $ Map.insert ((unsafeHead sd) ^. text) newId
            pure $ dict & nodeId .~ newId
