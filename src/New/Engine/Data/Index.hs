{-# LANGUAGE Strict #-}
module New.Engine.Data.Index where

import Prologue hiding (Index)

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map

import Data.Map.Strict (Map)


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

get :: State.Monad IndexMap m => m Index
get = do
    idxMap <- State.get @IndexMap
    let nextIndex = Index $! Map.size idxMap
    pure nextIndex
{-# INLINE get #-}

----------------------
-- === IndexMap === --
----------------------

-- === Definition === --

type IndexMap = Map Text Index
