{-# LANGUAGE PolyKinds #-}

module Type.Monoid where

infixr 6 <>
type a <> b = Concat a b
type family Concat (a :: k) (b :: k) :: k
