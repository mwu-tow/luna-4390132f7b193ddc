{-# LANGUAGE TypeInType #-}

module Data.Property where

import Control.Lens
import Data.Kind


-- === Definitions === --

-- FIXME[WD]: We can make TypeInType kind dependencies when GHC >= 8.0.2 cames out
type family Access  (t :: *) (a :: k) :: *
type family Update  (t :: *) (v :: *) (a :: *) :: *


-- === Utils === --

infixl 8 #
type a # t = Access t a

type Invariants2 t a = Update t (Access t a) a ~ a

class Accessor t a where access  ::                a -> Access t   a
class Updater  t a where update  :: forall v. v -> a -> Update t v a
class Updater' t a where update' ::  Access t a -> a -> a
                         default update' :: (Updater t a, Invariants2 t a) => Access t a -> a -> a
                         update' = update @t ; {-# INLINE update' #-}


-- === Properties === --

type family Properties a :: [*]

type Property  t a = forall b. Lens  a (Update t b a) (Access t a) b
type Property' t a =           Lens' a (Access t a)

type HasProperty  t a = (Accessor t a, Updater  t a)
type HasProperty' t a = (Accessor t a, Updater' t a, Invariants2 t a)

prop :: forall t a. HasProperty t a => Property t a
prop = lens (access @t) (flip $ update @t)

prop' :: forall t a. HasProperty' t a => Property' t a
prop' = lens (access @t) (flip $ update' @t)


-- === Instances === --

type instance Access t (Identity a) = Access t a
