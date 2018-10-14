module Searcher.Engine.Data.Symbol.Library
    ( module Searcher.Engine.Data.Symbol.Library
    , module X
    ) where

import Searcher.Engine.Prelude as X (Name)

import Searcher.Engine.Prelude

import qualified Searcher.Engine.Data.Score as Score


data Library = Library
    { _name     :: Name
    , _imported :: Bool
    } deriving (Eq, Show)

makeLenses ''Library


importedBonus :: Score.Fixed
importedBonus = 100

notImportedPenalty :: Score.Fixed
notImportedPenalty = 0

