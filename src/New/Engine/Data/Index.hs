{-# LANGUAGE Strict #-}
module New.Engine.Data.Index where

import Prologue hiding (Index)

import qualified Control.Monad.State.Layered as State

import Data.Map (Map)


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

get :: State.Monad Index m => m Index
get = State.modify @Index (\prev -> let curr = prev + 1 in (curr,curr))
{-# INLINE get #-}

----------------------
-- === IndexMap === --
----------------------

-- === Definition === --

type IndexMap = Map Text Index
