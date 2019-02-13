{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

module Type.Applicative (module Type.Applicative, module X) where

import Type.Functor as X
import Type.Monoid


infixl 4 <*>
type a <*> b = AppBind a b

type family AppBind a b where
            AppBind a '[] = '[]
            AppBind a (b ': bs) = AppBind' a b <> AppBind a bs

type family AppBind' a b where
            AppBind' '[]       b = '[]
            AppBind' (a ': as) b = a b ': AppBind' as b
