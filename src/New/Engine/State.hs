{-# LANGUAGE Strict #-}
module New.Engine.State where

import Prologue

import New.Engine.Data.Match (Match)
import Data.Text (Text)


-------------------
-- === State === --
-------------------


-- === Definition === --

data State = State
    { _remainingSuffix :: Text
    , _currentMatch    :: Match
    , _positionInQuery :: Int
    , _positionInData  :: Int
    } deriving (Eq, Generic, Show)
makeLenses ''State

instance NFData State

mk :: Text -> State
mk = \query -> State query def def def
{-# INLINE mk #-}
