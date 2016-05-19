module Graphics.API.Shader where

import           Control.Lens
import           Data.Aeson   (ToJSON, FromJSON)
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

import           Graphics.API.Component

data Shader = Shader { _components :: [Component]
                     } deriving (Show, Eq, Generic)

instance Binary Shader

instance ToJSON Shader
instance FromJSON Shader
