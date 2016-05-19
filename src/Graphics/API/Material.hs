module Graphics.API.Material where

import           Control.Lens
import           Data.Aeson   (ToJSON, FromJSON)
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

data Color = Color { _r :: Double
                   , _g :: Double
                   , _b :: Double
                   , _a :: Double
                   } deriving (Show, Eq, Generic)


makeLenses ''Color

instance Binary Color

instance ToJSON Color
instance FromJSON Color
