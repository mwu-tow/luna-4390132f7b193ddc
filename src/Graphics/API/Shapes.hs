module Graphics.API.Shapes where

import           Control.Lens
import           Data.Aeson   (ToJSON, FromJSON)
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

data Shape = Square    { _s :: Double }
           | Rectangle { _w :: Double
                       , _h :: Double }
           | Circle    { _d :: Double }
           deriving (Show, Eq, Generic)

instance Binary Shape

instance ToJSON Shape
instance FromJSON Shape
