
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables            #-}

module Type.List where

import Data.Typeable
import Prelude
import GHC.Exts (Constraint)
import GHC.TypeLits
import Type.Bool
import Type.Container
import qualified Type.Set as Set

-- === Wrappers ===

data Lst (l :: [k])

type family FromLst a where FromLst (Lst l) = l

-- === Basic operations ===

type family Removed    (el :: e)    (cont :: c) :: l
type family RemovedIdx (idx :: Nat) (cont :: c) :: l
type family ElAt       (idx :: Nat) (cont :: c) :: l 

type instance Removed (el :: e) ((l ': ls) :: [e]) = If (el == l) ls (l ': Removed el ls)


type instance RemovedIdx idx (l ': ls) = If (idx == 0) ls (l ': RemovedIdx (idx - 1) ls)

type instance ElAt idx (l ': ls) = If (idx == 0) l (ElAt (idx - 1) ls)


type instance In a (l ': ls) = If (a == l) True (In a ls)
type instance In a '[]       = False


type family SuccMaybe (m :: Maybe Nat) where SuccMaybe (Just n) = Just (n + 1)
                                             SuccMaybe Nothing  = Nothing

type instance Index a (l ': ls) = If (a == l) (Just 0) (SuccMaybe (Index a ls))
type instance Index a '[]       = (Nothing :: Maybe Nat)


data Recursive a

type instance Index a (Recursive (l ': ls)) = If (a == l) (Just 0) (SuccMaybe (Index a (Recursive ls)))
type instance Index a (Recursive '[]      ) = (Nothing :: Maybe Nat)


--type family Index2 (idx :: i) (cont :: c) :: el

type instance Index2 n (a ': as) = If (n == 0) a (Index2 (n - 1) as)


type instance Empty '[]       = 'True
type instance Empty (a ': as) = 'False


type instance Append a '[]       = '[a]
type instance Append a (l ': ls) = l ': Append a ls


type instance Concat (lst :: [k]) ('[]       :: [k]) = lst
type instance Concat (lst :: [k]) ((l ': ls) :: [k]) = Concat (Append l lst) ls


type instance Remove a '[] = '[]
type instance Remove a (l ': ls) = If (a == l) ls (l ': Remove a ls)


type instance Diff l '[]       = l
type instance Diff l (e ': es) = Diff (Remove e l) es

type instance Size '[]       = 0
type instance Size (a ': as) = 1 + Size as

type family Head lst where
    Head '[]       = 'Nothing
    Head (a ': as) = 'Just a

type family Head' lst where Head' (a ': as) = a

--type instance Unique (lst :: [k]) = Set.ToList (Set.AsSet lst)

-- FIXME[WD]: The following is just an fix for using Unique. 
--            Due to lack of time this ugly fix has no background - probably some PolyKinded problem.
type family UniqueFix lst where UniqueFix '[] = '[]
                                UniqueFix lst = Unique lst
type instance Unique (lst :: [k]) = Set.ToList (Set.AsSet lst)

type instance Reverse (lst :: [k]) = Reverse' lst '[]

type family Reverse' lst lst' where
    Reverse' '[] lst = lst
    Reverse' (l ': ls) lst = Reverse' ls (l ': lst)


type family Take (n :: Nat) lst where
	Take 0 lst       = '[]
	Take n (l ': ls) = l ': Take (n - 1) ls

--type instance Index (a :: *) (Recursive ((l ': ls) ::  [*] )) = If (a == l) (Just 0) (SuccMaybe (Index a (Recursive ls)))
--type instance Index (a :: *) (Recursive ((l ': ls) :: [[*]])) = If (Index a l == 'Nothing) (SuccMaybe (Index a (Recursive ls))) ('Just 0)
--type instance Index a '[]                                     = 'Nothing


--type instance RecIndex a ((l ': ls) :: [[*]]) = If (RecIndex a l == 'Nothing) (SuccMaybe (RecIndex a ls)) (Just 0)

