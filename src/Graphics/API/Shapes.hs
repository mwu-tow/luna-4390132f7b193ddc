module Graphics.API.Shapes where

import           Control.Lens
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

data Square = Square { _s :: Double } deriving (Show, Eq, Generic)

data Rectangle = Rectangle { _w :: Double
                           , _h :: Double
                           } deriving (Show, Eq, Generic)

data Circle = Circle { _d :: Double } deriving (Show, Eq, Generic)

makeLenses ''Square
makeLenses ''Rectangle
makeLenses ''Circle

instance Binary Square
instance Binary Rectangle
instance Binary Circle
