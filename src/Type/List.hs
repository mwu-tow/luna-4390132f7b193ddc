
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Type.List where

import Data.Typeable
import Prelude
import GHC.Exts (Constraint)
import GHC.TypeLits
import Type.Bool

-- === Basic operations ===

type family Removed    (el :: e) (cont :: c) :: l
type family RemovedIdx (idx :: Nat) (cont :: c) :: l
type family ElAt       (idx :: Nat) (cont :: c) :: l 

type instance Removed (el :: e) ((l ': ls) :: [e]) = If (el :== l) ls (l ': Removed el ls)


type instance RemovedIdx idx (l ': ls) = If (idx :== 0) ls (l ': RemovedIdx (idx - 1) ls)

type instance ElAt idx (l ': ls) = If (idx :== 0) l (ElAt (idx - 1) ls)