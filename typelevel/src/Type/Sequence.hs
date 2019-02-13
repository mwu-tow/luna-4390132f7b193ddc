{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Sequence where

import Prelude
import GHC.TypeLits

type family Succ (a :: k) :: k

type instance Succ (a :: Nat) = a + 1


type family Empty :: k

type instance Empty = '[]


type family Zero :: k

type instance Zero = 0



type family Range (begin :: k) (end :: k) :: [k] where
    Range b b = Empty
    Range b e = b ': Range (Succ b) e

type family Enumerate end where Enumerate end = Range Zero end -- Implemented as TF because #11375
