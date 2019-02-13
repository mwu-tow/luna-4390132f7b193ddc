{-# LANGUAGE PolyKinds #-}

module Type.Relation where


type family Super (a :: k) :: [k]


type SemiSuper a = a ': Super a
