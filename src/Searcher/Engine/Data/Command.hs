module Searcher.Engine.Data.Command
    ( module Searcher.Engine.Data.Command
    , module X
    ) where

import Searcher.Engine.Data    as X
import Searcher.Engine.Prelude as X (Documentation, Name)

import Searcher.Engine.Prelude


data Command = Command
    { _commandName          :: Name
    , _commandDocumentation :: Documentation
    } deriving (Eq, Show)

makeLenses ''Command


instance SearcherData Command where
    name          = commandName
    documentation = commandDocumentation
    prefix        = to $ const mempty
    score         = to $ const def
