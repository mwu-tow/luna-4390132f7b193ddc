module Graphics.API.Graphics where

import           Control.Lens
import           Data.Aeson   (ToJSON, FromJSON)
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

import           Graphics.API.Layer

data Graphics = Graphics { _graphics :: [Layer]
                         } deriving (Show, Eq, Generic)

instance Binary Graphics

instance ToJSON Graphics
instance FromJSON Graphics
