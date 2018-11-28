{-# LANGUAGE Strict #-}

module New.Engine.Metric where

import Prologue

import New.Engine.Data.Score as Score

-- TODO [Ara] Design for metrics
-- Idea for metric as a typeclass, with type-applied arguments.
-- Can be compile-time only.
-- Needs next letter in query and next letter in database.
-- Return current score. (as Int)
-- Can potentially be stateful.
-- e.g. Prefix matcher contains state about collected points, multipliers and
-- context info.



--------------------
-- === Metric === --
--------------------

-- === Definition === --


-- === API === --


-- === Instances === --



-------------------------
-- === Test Metric === --
-------------------------


-------------------------------
-- === Utility Functions === --
-------------------------------

testMetrics :: IO ()
testMetrics = print ("Foo" :: String)

