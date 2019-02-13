{-# LANGUAGE RankNTypes #-}

module Old.Data.Attr where

import Prelude
import Control.Lens hiding (Getter, Setter)
import Data.Typeable
import Data.Layer_OLD
import Data.Layer_OLD.Cover_OLD
import Data.Impossible



-- === Definition === ---

-- Attribute result depends mainly on the attribute type
-- and we can define `Attr` instances for given `a` and ANY `t`
type family   Attr a t
type instance Attr Impossible t = Impossible


-- === Type classes === ---

class Getter a t where
    getter :: a -> t -> Attr a t
    default getter :: SubGetter a t => a -> t -> Attr a t
    getter a = getter a . unlayer ; {-# INLINE getter #-}

class Setter a t where
    setter :: a -> Attr a t -> t -> t
    default setter :: SubSetter a t => a -> Attr a t -> t -> t
    setter a v t = t & layered %~ setter a v

type HasAttr   a t = (Getter a t, Setter a t)
type SubGetter a t = (Layered t, Getter a (Unlayered t), Attr a t ~ Attr a (Unlayered t))
type SubSetter a t = (Layered t, Setter a (Unlayered t), Attr a t ~ Attr a (Unlayered t))


-- === Utils === --

attr :: HasAttr a t => a -> Lens' t (Attr a t)
attr a = lens (getter a) (flip $ setter a)
