
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Strict    #-}

module Searcher.Engine.Metric where

import Prologue hiding (Monad)

import qualified Searcher.Engine.Data.Match  as Match

import Searcher.Engine.Data.Score (Score)



--------------------
-- === Metric === --
--------------------

-- === Definition === --

-- TODO [Ara] Use TypeMap to autogenerate the boilerplate for this.

class (Default a, NFData a) => Metric a where
    updateMetric :: a -> Char -> Match.CharMatch -> Match.State -> a
    getMetric :: a -> Match.State -> Score

type family Metrics ss :: Constraint where
    Metrics (s ': ss) = (Metric s, Metrics ss)
    Metrics '[]       = ()

