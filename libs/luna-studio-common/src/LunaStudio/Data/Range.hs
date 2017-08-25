{-# LANGUAGE DeriveAnyClass #-}
module LunaStudio.Data.Range where

import           Data.Binary (Binary)
import           Prologue


data Range = Range
        { _start :: Int
        , _end   :: Int
        } deriving (Binary, Eq, Generic, NFData, Show)

makeLenses ''Range

instance Convertible (Int, Int) Range where
    convert = uncurry Range
