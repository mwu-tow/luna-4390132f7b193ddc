
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Type.Wrapped where

import Data.Typeable
import Prelude
import GHC.Exts (Constraint)
import GHC.TypeLits
import Type.Bool
import Type.Container

-- === Basic operations ===

type family Unwrapped (a :: k) :: l