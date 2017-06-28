{-# LANGUAGE DeriveAnyClass #-}
module LunaStudio.Data.Point where

import           Data.Binary (Binary)
import           Prologue


data Point = Point
        { _column :: Int
        , _row    :: Int
        } deriving (Binary, Eq, Generic, NFData, Show)

makeLenses ''Point
