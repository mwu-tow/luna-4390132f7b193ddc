--TODO: Refactor this file, Position and Vector2 to make more functions common
{-# LANGUAGE TypeFamilies #-}
module LunaStudio.Data.ScreenPosition
    ( module LunaStudio.Data.ScreenPosition
    , vector
    , x
    , y
    ) where

import           LunaStudio.Data.Vector2
import           Prologue


-----------------------------
-- === ScreenPosition === ---
-----------------------------

-- === Definition === --

newtype ScreenPosition = ScreenPosition { fromScreenPosition :: Vector2 Double } deriving (Eq, Show, Generic, Default, NFData, Num)
makeWrapped ''ScreenPosition

-- instance Num (ScreenPosition) where
--     (+) = undefined

-- === Instances === --

type instance VectorOf ScreenPosition = Vector2 Double

instance Dim1      ScreenPosition
instance Dim2      ScreenPosition
instance IsVector  ScreenPosition

type instance Item ScreenPosition = Double
instance Convertible ScreenPosition [Double] where convert = toList . view vector
instance Convertible [Double] ScreenPosition where convert = ScreenPosition . fromList


-- === Functions === ---

fromTuple :: (Double, Double) -> ScreenPosition
fromTuple = uncurry fromDoubles

fromDoubles :: Double -> Double -> ScreenPosition
fromDoubles = ScreenPosition .: Vector2

toTuple :: ScreenPosition -> (Double, Double)
toTuple (ScreenPosition (Vector2 x' y')) = (x', y')

move :: Vector2 Double -> ScreenPosition -> ScreenPosition
move vec pos = pos & vector +~ vec
