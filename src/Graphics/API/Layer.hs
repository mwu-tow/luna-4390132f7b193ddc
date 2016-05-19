module Graphics.API.Layer where

import           Control.Lens
import           Data.Aeson   (ToJSON, FromJSON)
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

import           Graphics.API.Shader
import           Graphics.API.Transformation

data Layer = Layer { _shader          :: Shader
                   , _transformations :: [Transformation]
                   } deriving (Show, Eq, Generic)

instance Binary Layer

instance ToJSON Layer
instance FromJSON Layer
