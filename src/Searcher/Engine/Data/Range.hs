module Searcher.Engine.Data.Range where

import Searcher.Engine.Prelude

import Control.Arrow ((&&&))


type Range = (Int, Int)


characterRange :: Int -> Range
characterRange = id &&& succ

appendRange :: Range -> [Range] -> [Range]
appendRange i [] = [i]
appendRange i@(newBeg, newEnd) prev@((oldBeg, oldEnd) : t)
    = if oldEnd == newBeg
        then (oldBeg, newEnd) : t
        else i : prev
