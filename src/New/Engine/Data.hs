{-# LANGUAGE Strict #-}
module New.Engine.Data where

import Prologue

import Control.Lens (Getter, to)
import Data.Text    (Text)


--------------------------
-- === SearcherData === --
--------------------------

-- === Definition === --

class Eq a => SearcherData a where
    text :: Getter a Text


-- === Instances === ---

instance SearcherData Text where
    text = to id
