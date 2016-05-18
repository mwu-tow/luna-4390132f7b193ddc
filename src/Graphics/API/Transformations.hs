module Graphics.API.Transformations where

import           Control.Lens
import           Data.Aeson   (ToJSON)
import           Data.Binary  (Binary)
import           Data.Default
import           GHC.Generics (Generic)

data Transformation = Transformation { _dx    :: Double
                                     , _dy    :: Double
                                     , _angle :: Double
                                     , _refl  :: Bool
                                     } deriving (Show, Eq, Generic)

instance Default Transformation where
    def = Transformation 0.0 0.0 0.0 False

makeLenses ''Transformation

instance Binary Transformation
instance ToJSON Transformation
