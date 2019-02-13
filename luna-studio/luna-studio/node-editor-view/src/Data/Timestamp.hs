{-# LANGUAGE TypeFamilies #-}
module Data.Timestamp where

import           Common.Prelude
import           Control.DeepSeq (NFData)
import           Data.Aeson      (FromJSON, ToJSON)


newtype Timestamp = Timestamp Int deriving (Enum, Eq, Generic, Integral, NFData, Num, Ord, Real, Show)

instance ToJSON Timestamp
instance FromJSON Timestamp
makeWrapped ''Timestamp
