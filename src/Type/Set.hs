
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Type.Set where

import Data.Typeable
import Prelude
import GHC.Exts (Constraint)
import GHC.TypeLits
import Type.Bool
import Type.Container
import Type.Wrapped
import Type.Operators


-- === Declarations ===

data Set (a :: [k])

type instance Unwrapped (Set a) = a


-- === Conversions ===

type family AsSet  (a :: k)
type family AsSet' (a :: k)

type instance AsSet (lst :: [k]) = AsSet' (Reverse lst)
type instance AsSet' '[]         = Set '[]
type instance AsSet' (l ': ls)   = Insert l (AsSet' ls)

type family ToList s where ToList (Set s) = s



-- === Operations ===


type instance Insert a (Set '[])       = Set '[a]
type instance Insert a (Set (s ': ss)) = Set $ If (a == s) (s ': ss) (s ': Unwrapped (Insert a (Set ss)))

type instance Concat (Set set) (Set '[]) = Set set
type instance Concat (Set set) (Set (s ': ss)) = If (s `In` set) (Concat (Set set) (Set ss)) (Concat (Insert s (Set set)) (Set ss))

type instance Index a (Set s) = Index a s