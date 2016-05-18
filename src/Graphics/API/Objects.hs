module Graphics.API.Objects where

import           Control.Lens
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

import           Graphics.API.Shapes
import           Graphics.API.Materials
import           Graphics.API.Transformations


data Object = Object { _shape          :: Shape
                     , _color          :: Color
                     , _transformation :: Transformation
                     } deriving (Show, Eq, Generic)


makeLenses ''Object

instance Binary Object
