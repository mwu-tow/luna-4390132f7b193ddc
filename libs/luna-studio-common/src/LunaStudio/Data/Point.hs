{-# LANGUAGE DeriveAnyClass #-}
module LunaStudio.Data.Point where

import           Data.Aeson.Types (FromJSON, ToJSON)
import           Data.Binary      (Binary)
import           Prologue


data Point = Point
        { _column :: Int
        , _row    :: Int
        } deriving (Binary, Eq, Generic, NFData, Show)

makeLenses ''Point
instance FromJSON Point
instance ToJSON   Point
instance Convertible (Int, Int) Point where
    convert = uncurry Point
