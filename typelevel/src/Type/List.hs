
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables            #-}

module Type.List (module Type.List, module X) where

import Prelude
import GHC.TypeLits
import Type.Bool
import Type.Container as X
import qualified Type.Set as Set
import Type.Monoid as X

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


type instance In a (l ': ls) = If (a == l) 'True (In a ls)
type instance In a '[]       = 'False


type family SuccMaybe (m :: Maybe Nat) where SuccMaybe ('Just n) = 'Just (n + 1)
                                             SuccMaybe 'Nothing  = 'Nothing

type instance Index a (l ': ls) = If (a == l) ('Just 0) (SuccMaybe (Index a ls))
type instance Index a '[]       = ('Nothing :: Maybe Nat)

type instance IndexF a (l ': ls) = If (a == l) 0 (IndexF a ls + 1)


data Recursive a

-- TODO: rename to IndexOf
type instance Index a (Recursive (l ': ls)) = If (a == l) ('Just 0) (SuccMaybe (Index a (Recursive ls)))
type instance Index a (Recursive '[]      ) = ('Nothing :: Maybe Nat)


--type family Index2 (idx :: i) (cont :: c) :: el

type instance Index2 n (a ': as) = If (n == 0) a (Index2 (n - 1) as)


type instance Empty '[]       = 'True
type instance Empty (a ': as) = 'False


type instance Append a '[]       = '[a]
type instance Append a (l ': ls) = l ': Append a ls


type instance Concat ('[] :: [k])        (lst :: [k]) = lst
type instance Concat ( (l ': ls) :: [k]) (lst :: [k]) = l ': Concat ls lst


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
    Reverse' '[]       lst = lst
    Reverse' (l ': ls) lst = Reverse' ls (l ': lst)


type family Take (n :: Nat) lst where
    Take 0 lst       = '[]
    Take n (l ': ls) = l ': Take (n - 1) ls

type family Drop (n :: Nat) lst where
    Drop 0 lst       = lst
    Drop n (l ': ls) = Drop (n - 1) ls

--type instance Index (a :: *) (Recursive ((l ': ls) ::  [*] )) = If (a == l) (Just 0) (SuccMaybe (Index a (Recursive ls)))
--type instance Index (a :: *) (Recursive ((l ': ls) :: [[*]])) = If (Index a l == 'Nothing) (SuccMaybe (Index a (Recursive ls))) ('Just 0)
--type instance Index a '[]                                     = 'Nothing


--type instance RecIndex a ((l ': ls) :: [[*]]) = If (RecIndex a l == 'Nothing) (SuccMaybe (RecIndex a ls)) (Just 0)

type family Join (lst :: [[k]]) :: [k] where
    Join '[]           = '[]
    Join (lst ': lsts) = Concat lst (Join lsts)

type family Last (lst :: [k]) :: k where
    Last '[x]      = x
    Last (x ': xs) = Last xs


type family Init (lst :: [k]) :: [k] where
    Init '[]       = '[]
    Init '[x]      = '[]
    Init (x ': xs) = x ': Init xs

type family DropInit (lst :: [k]) :: [k] where
    DropInit '[]       = '[]
    DropInit '[x]      = '[x]
    DropInit (x ': xs) = DropInit xs

type family Replicate (n :: Nat) a where
    Replicate 0 a = '[]
    Replicate n a = a ': Replicate (n - 1) a


type Zip a b = Zip2 a b

type family Zip2 (l1 :: [*]) (l2 :: [*]) :: [*] where
    Zip2 (x1 ': xs1) (x2 ': xs2) = (x1,x2) ': Zip2 xs1 xs2
    Zip2 l1          l2          = '[]

type family Zip3 (l1 :: [*]) (l2 :: [*]) (l3 :: [*]) :: [*] where
    Zip3 (x1 ': xs1) (x2 ': xs2) (x3 ': xs3) = (x1,x2,x3) ': Zip3 xs1 xs2 xs3
    Zip3 l1          l2          l3          = '[]

type family Zip4 (l1 :: [*]) (l2 :: [*]) (l3 :: [*]) (l4 :: [*]) :: [*] where
    Zip4 (x1 ': xs1) (x2 ': xs2) (x3 ': xs3) (x4 ': xs4) = (x1,x2,x3,x4) ': Zip4 xs1 xs2 xs3 xs4
    Zip4 l1          l2          l3          l4          = '[]

type family Zip5 (l1 :: [*]) (l2 :: [*]) (l3 :: [*]) (l4 :: [*]) (l5 :: [*]) :: [*] where
    Zip5 (x1 ': xs1) (x2 ': xs2) (x3 ': xs3) (x4 ': xs4) (x5 ': xs5) = (x1,x2,x3,x4,x5) ': Zip5 xs1 xs2 xs3 xs4 xs5
    Zip5 l1          l2          l3          l4          l5          = '[]


type family Unzip2 (lst :: [*]) :: ([*], [*]) where
    Unzip2 '[]              = '( '[], '[] )
    Unzip2 ((x1,x2) ': lst) = PrependAll (x1,x2) (Unzip2 lst)

--type family Unzip3 (lst :: [*]) :: ([*], [*], [*]) where
--    Unzip3 '[]              = '( '[], '[] )
--    Unzip3 ((x1,x2) ': lst) = PrependAll (x1,x2) (Unzip3 lst)


type family PrependAll (els :: *) (lsts :: k) :: k
type instance PrependAll (t1,t2) '(l1,l2) = '(t1 ': l1, t2 ': l2)
type instance PrependAll (t1,t2,t3) '(l1,l2,l3) = '(t1 ': l1, t2 ': l2, t3 ': l3)
type instance PrependAll (t1,t2,t3,t4) '(l1,l2,l3,l4) = '(t1 ': l1, t2 ': l2, t3 ': l3, t4 ': l4)
type instance PrependAll (t1,t2,t3,t4,t5) '(l1,l2,l3,l4,l5) = '(t1 ': l1, t2 ': l2, t3 ': l3, t4 ': l4, t5 ': l5)


type family Select (n :: Nat) (a :: *) :: *

type instance Select 1 (t1,t2) = t1
type instance Select 2 (t1,t2) = t2

type instance Select 1 (t1,t2,t3) = t1
type instance Select 2 (t1,t2,t3) = t2
type instance Select 3 (t1,t2,t3) = t3

type instance Select 1 (t1,t2,t3,t4) = t1
type instance Select 2 (t1,t2,t3,t4) = t2
type instance Select 3 (t1,t2,t3,t4) = t3
type instance Select 4 (t1,t2,t3,t4) = t4

type instance Select 1 (t1,t2,t3,t4,t5) = t1
type instance Select 2 (t1,t2,t3,t4,t5) = t2
type instance Select 3 (t1,t2,t3,t4,t5) = t3
type instance Select 4 (t1,t2,t3,t4,t5) = t4
type instance Select 5 (t1,t2,t3,t4,t5) = t5



type family Update (n :: Nat) (a :: k) (lst :: [k]) :: [k] where
    Update 0 a (l ': ls) = a ': ls
    Update n a (l ': ls) = l ': Update (n - 1) a ls

type family TakeUntil (a :: k) (ls :: [k]) :: [k] where
    TakeUntil a '[]       = '[]
    TakeUntil a (a ': ls) = '[a]
    TakeUntil a (l ': ls) = l ': TakeUntil a ls
