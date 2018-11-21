module New.Engine.Prelude
    ( module New.Engine.Prelude
    , module X
    ) where

import Control.Arrow as X ((&&&))
import Control.Lens  as X (Getter, makePrisms, to, (?~))
import Prologue      as X hiding (Symbol)

import qualified Data.Text as Text


type Query = Text

textHead :: Text -> Maybe Char
textHead t = if Text.null t then Nothing else Just $ Text.head t
