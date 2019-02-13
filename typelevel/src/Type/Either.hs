{-# LANGUAGE PolyKinds #-}

module Type.Either where

import Prelude


type family IsLeft (a :: Either l r) :: Bool where IsLeft ('Left  l) = 'True
                                                   IsLeft ('Right r) = 'False

type family IsRight (a :: Either l r) :: Bool where IsRight ('Left  l) = 'False
                                                    IsRight ('Right r) = 'True


type family FromRight (a :: Either l r) where FromRight ('Right r) = r
type family FromLeft  (a :: Either l r) where FromLeft  ('Left  l) = l