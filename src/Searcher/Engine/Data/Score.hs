{-# LANGUAGE Strict #-}

module Searcher.Engine.Data.Score where

import Prologue

import Control.Lens.TH as Lens



-------------------
-- === Score === --
-------------------

-- === Definition === --

newtype Score = Score Int deriving (Eq, Generic, Num, Ord, Show)
Lens.makeWrapped ''Score


-- === Instances === --

instance Default Score where
    def = Score (def @Int)

instance NFData Score

