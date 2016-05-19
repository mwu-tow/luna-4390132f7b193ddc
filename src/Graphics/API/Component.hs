module Graphics.API.Component where

import           Control.Lens
import           Data.Aeson   (ToJSON, FromJSON)
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

import           Graphics.API.Shape
import           Graphics.API.Material

data Component = Component { _shape :: Shape
                           , _color :: Color
                           } deriving (Show, Eq, Generic)

instance Binary Component

instance ToJSON Component
instance FromJSON Component
