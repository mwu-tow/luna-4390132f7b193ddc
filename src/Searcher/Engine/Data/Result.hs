{-# LANGUAGE Strict #-}
module Searcher.Engine.Data.Result where

import Prologue hiding (Index)

import qualified Searcher.Engine.Data.Database as Database
import qualified Searcher.Engine.Data.Match    as Match

import Control.Lens             (Getter, to)
import Searcher.Engine.Data.Database (SearcherData)
import Searcher.Engine.Data.Match    (Match)
import Searcher.Engine.Data.Score    (Score)



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

score :: SearcherData a => Getter (Result a) Score
score = to $! \r -> let
    points = r ^. match . Match.points
    hint'  = r ^. hint
    in Database.calculateScore points hint'
