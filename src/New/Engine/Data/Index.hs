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

get :: State.Monad TextMap m => m Index
get = do
    txtMap <- State.get @TextMap
    let nextIndex = Index $! Map.size txtMap
    pure nextIndex
{-# INLINE get #-}



---------------------
-- === TextMap === --
---------------------


-- === Definition === --

type TextMap = Map Index Text
