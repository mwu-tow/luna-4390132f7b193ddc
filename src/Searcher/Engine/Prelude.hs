module Searcher.Engine.Prelude
    ( module Searcher.Engine.Prelude
    , module X
    ) where

import Control.Lens as X (Getter, makePrisms, to, (?~))
import Data.List    as X (sort)
import Prologue     as X hiding (Symbol)

import Data.Text (Text)


type Name          = Text
type Prefix        = Name
type Documentation = Text
type Query         = Text

data MatchKind
    = CaseSensitiveEquality
    | CaseInsensitiveEquality
    | AllCharsMatched
    | NotFullyMatched
    deriving (Eq, Ord, Show)

makePrisms ''MatchKind
