{-# LANGUAGE Strict #-}
module Searcher.Engine.Data.Result where

import Prologue hiding (Index)

import qualified Searcher.Engine.Data.Database as Database
import qualified Searcher.Engine.Data.Match    as Match

import Control.Lens                  (Getter, to)
import Searcher.Engine.Data.Database (SearcherData (fixedScore, text))
import Searcher.Engine.Data.Match    (Match)



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
instance SearcherData a => SearcherData (Result a) where
    text       = hint . text
    fixedScore = hint . fixedScore



getScore :: SearcherData a => (a -> Double) -> Result a -> Double
getScore weightGetter results = let
    points = results ^. match . Match.points
    hint'  = results ^. hint
    in Database.calculateScore points weightGetter hint'
{-# INLINE getScore #-}
