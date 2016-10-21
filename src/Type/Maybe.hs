{-# LANGUAGE PolyKinds #-}

module Type.Maybe where

import Prelude

type family IsJust a where IsJust ('Just a) = 'True
                           IsJust a         = 'False


type family CatMaybes (lst :: [Maybe k]) :: [k] where
    CatMaybes '[]              = '[]
    CatMaybes ('Just a  ': ms) = a ': CatMaybes ms
    CatMaybes ('Nothing ': ms) =      CatMaybes ms

type family FromJust (m :: Maybe k) :: k where
    FromJust ('Just a) = a
