{-# LANGUAGE Strict #-}

module New.Engine.Data.Score where

import Prologue



-------------------
-- === Score === --
-------------------

-- === Definition === --

newtype Score = Score Int deriving (Eq, Generic, Num, Ord, Show)


-- === Instances === --

instance Default Score where
    def = Score (def @Int)

instance NFData Score

