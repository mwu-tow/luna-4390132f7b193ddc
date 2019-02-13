module NodeEditor.Data.Slider where

import Common.Prelude
import Data.Aeson     (FromJSON, ToJSON)


data InitValue = Discrete  Integer
               | Continous Double
               deriving (Eq, Show, Generic, Typeable)

instance NFData InitValue

makeLenses ''InitValue

instance ToJSON InitValue
instance FromJSON InitValue
