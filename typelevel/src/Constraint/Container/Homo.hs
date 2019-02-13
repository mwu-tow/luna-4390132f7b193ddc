{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Constraint.Container.Homo where

import Data.Typeable
import Prelude
import GHC.Exts (Constraint)
import GHC.TypeLits
import Type.Bool

-- === Constraints ===

type family Homo (a :: *) :: Constraint

-- TODO [WD] TH
type instance Homo (t,t2) = (t~t2)
type instance Homo (t,t2,t3) = (t~t2, t~t3)
type instance Homo (t,t2,t3,t4) = (t~t2, t~t3, t~t4)
type instance Homo (t,t2,t3,t4,t5) = (t~t2, t~t3, t~t4, t~t5)
-- ...