module Data.Constraints where

import Data.Constraint


type family Constraints (cs :: [Constraint]) :: Constraint where
    Constraints '[]       = ()
    Constraints (c ': cs) = (c , Constraints cs)
