{-# LANGUAGE Strict #-}

module New.Engine.Data.Match where

import Prologue hiding (Index)

import qualified New.Engine.Data.Substring as Substring

import New.Engine.Data.Substring (Substring)



-------------------
-- === Match === --
-------------------

-- === Definition === --

data Match = Match
    { _substring :: Substring
    , _kind      :: Substring.Kind -- TODO: This should be converted into points and removed
    , _points    :: Int
    } deriving (Eq, Generic, Show)
makeLenses ''Match

instance NFData  Match
instance Default Match where def = Match def Substring.Equal def
instance Ord     Match where
    -- TODO[LJK]: This should be replaced with scoring match kind as soon as old algorithm is recreated
    compare m1 m2 = (m1Kind, m1Points) `compare` (m2Kind, m2Points) where
        m1Kind   = m1 ^. kind
        m2Kind   = m2 ^. kind
        m2Points = m2 ^. points
        m1Points = m1 ^. points



-------------------
-- === State === --
-------------------

-- === Definition === --

data State = State
    { _remainingSuffix  :: Text
    , _currentSubstring :: Substring
    , _currentKind      :: Substring.Kind
    , _positionInQuery  :: Int
    , _positionInData   :: Int
    } deriving (Eq, Generic, Show)
makeLenses ''State

instance NFData State

mkState :: Text -> State
mkState = \query -> State query def Substring.Equal def def
{-# INLINE mkState #-}

