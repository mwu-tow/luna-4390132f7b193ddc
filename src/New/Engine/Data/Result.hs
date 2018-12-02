{-# LANGUAGE Strict #-}
module New.Engine.Data.Result where

import Prologue hiding (Index)

import qualified New.Engine.Data.Substring as Substring
import qualified New.Engine.Data.Database  as Database
import qualified New.Engine.Data.Index as Index

import Data.Map.Strict (Map)
import New.Engine.Data.Substring (Substring)
import New.Engine.Data.Index (Index)
import New.Engine.Data.Database (SearcherData)
import Control.Lens (Getter, to)



-------------------
-- === Match === --
-------------------


-- === Definition === --

data Match = Match
    { _matchedChars :: Substring
    , _matchKind    :: Substring.Kind -- TODO: This should be converted into points and removed
    , _matchPoints  :: Int
    } deriving (Eq, Generic, Show)
makeLenses ''Match

instance NFData Match
instance Ord    Match where
    -- TODO[LJK]: This should be replaced with scoring match kind as soon as old algorithm is recreated
    compare m1 m2 = (m1Kind, m1Points) `compare` (m2Kind, m2Points) where
        m1Kind   = m1 ^. matchKind
        m2Kind   = m2 ^. matchKind
        m2Points = m2 ^. matchPoints
        m1Points = m1 ^. matchPoints



--------------------
-- === Result === --
--------------------


-- === Definition === --

data Result a = Result
    { _hint  :: a
    , _match :: Match
    } deriving (Eq, Generic, Show)
makeLenses ''Result

instance NFData a => NFData (Result a)
instance SearcherData a => Ord (Result a) where
    compare r1 r2 = (r2 ^. score) `compare` (r1 ^. score)

score :: SearcherData a => Getter (Result a) Int
score = to $! \r -> let
    matchPoints' = r ^. match . matchPoints
    hint'        = r ^. hint
    in Database.calculateScore matchPoints' hint'
