{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Container where

import Prelude
import GHC.TypeLits


type family In     (el :: ke) (cont :: k) :: Bool
type family Index2 (idx :: i) (cont :: c) :: el
type family Index  (el :: ke) (cont :: k) :: Maybe Nat
type family Append (el :: ke) (cont :: k) :: k
type family Insert (el :: ke) (cont :: k) :: k
type family Remove (el :: ke) (cont :: k) :: k
type family Empty             (cont :: k) :: Bool
type family Size              (cont :: k) :: Nat
type family Reverse           (cont :: k) :: k
--type family Head              (cont :: k) :: Maybe *

type family Unique            (cont :: k) :: k



type family Diff  (c :: k) (c' :: k) :: k
type family Union (c :: k) (c' :: k) :: k

--infixr 6      :<>
--type family (a :: k) :<> (b :: k) where a :<> b = Concat a b





--------------------------------

type family FromJust a where FromJust ('Just a) = a
type UnsafeIndex el cont = FromJust (Index el cont)
