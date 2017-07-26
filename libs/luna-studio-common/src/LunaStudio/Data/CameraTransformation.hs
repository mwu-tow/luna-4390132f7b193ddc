{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LunaStudio.Data.CameraTransformation  where

import           Control.DeepSeq (NFData)
import           Data.Aeson      (FromJSON (..), ToJSON (..))
import           Data.Binary     (Binary (..))
import           Data.Matrix     (Matrix, fromLists, identity, toLists)
import           Prologue

--TODO[react]: Consider if we can require those Matrices to be squared and of size 4
data CameraTransformation = CameraTransformation { _screenToLogical :: Matrix Double
                                                 , _logicalToScreen :: Matrix Double
                                                 , _lastInverse     :: Int
                                                 } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

instance Binary a => Binary (Matrix a) where
    put = put . toLists
    get = fromLists . get

instance Binary CameraTransformation

instance ToJSON a => ToJSON (Matrix a) where
    toJSON = toJSON . toLists
instance FromJSON a => FromJSON (Matrix a) where
    parseJSON = fmap fromLists . parseJSON

makeLenses ''CameraTransformation

instance Default CameraTransformation where
    def = CameraTransformation (identity 4) (identity 4) 0
