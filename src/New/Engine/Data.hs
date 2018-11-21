{-# LANGUAGE Strict #-}
module New.Engine.Data where

import New.Engine.Prelude

import Data.Text (Text)


class Eq a => SearcherData a where
    text :: Lens' a Text 