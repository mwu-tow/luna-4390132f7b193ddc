{-# LANGUAGE Strict #-}
module New.Engine.Data.Result where

import Prologue hiding (Index)

import qualified New.Engine.Data.Database  as Database
import qualified New.Engine.Data.Match     as Match

import Control.Lens              (Getter, to)
import New.Engine.Data.Database  (SearcherData)
import New.Engine.Data.Match     (Match)



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
    points = r ^. match . Match.points
    hint'  = r ^. hint
    in Database.calculateScore points hint'
