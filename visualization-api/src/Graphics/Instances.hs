module Graphics.Instances where

import           Data.Matrix (Matrix, toLists, fromLists)
import           Data.Aeson  (FromJSON(parseJSON), ToJSON(toJSON))
import           Data.Binary (Binary(get,put))



instance Binary   (Matrix Double) where
    put = put . toLists
    get = fmap fromLists get

instance FromJSON (Matrix Double) where
    parseJSON = fmap fromLists . parseJSON

instance ToJSON   (Matrix Double) where
    toJSON = toJSON . toLists
