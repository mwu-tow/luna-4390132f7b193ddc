{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes           #-}

module Old.Data.Prop where

import Prelude
import Control.Lens hiding (Getter, Setter)
import Data.Typeable
import Data.Layer_OLD
import Data.Layer_OLD.Cover_OLD
import Data.Impossible



-- === Definition === ---

-- Properties result depends mainly on the base type `t`
-- and we can define `Prop` instances for ANY `p` given particular `t`.
type family   Prop p t
type instance Prop p Impossible = Impossible


-- === Type classes === ---

class Getter p t where
    getter :: p -> t -> t # p
    default getter :: SubGetter p t => p -> t -> t # p
    getter p = getter p . unlayer ; {-# INLINE getter #-}

class Setter p t where
    setter :: p -> Prop p t -> t -> t
    default setter :: SubSetter p t => p -> Prop p t -> t -> t
    setter p v t = t & layered %~ setter p v



data Single = Single deriving (Show)
data Every  = Every  deriving (Show)

data Selector div a = Selector deriving (Show)

class Getter2 div a t where
    getter2 :: Selector div a -> t -> Diversity div (t # a)


type family Attr selector a t

type instance Attr Single a t = Prop a t
type instance Attr Every  a t = [Prop a t]


type family Diversity d a
type instance Diversity Single a = a
type instance Diversity Every  a = [a]


type TransAttr sel p a b = (Attr sel p a ~ Attr sel p b)
type TransAttr' sel p a = (Attr sel p a ~ Attr sel p (Unwrapped a))

type UnwrappedGetter sel p a = (Getter2 sel p (Unwrapped a), TransAttr' sel p a)
--getter2 Every Input



--type family Response r a

--type instance Response (Selector Single a) = Prop

--type family Attr a t

--type instance Attr (Single p) t = Prop p t
--type instance Attr (Single p) t = Prop p t

--class GetProp p t where
--    getProp :: p -> t -> Prop p t
--    default getProp :: HasProp2 p t => p -> t -> Prop p t
--    getProp = view . prop2

--class SetProp p t where
--    setProp :: p -> Prop p t -> t -> t
--    default setProp :: HasProp2 p t => p -> Prop p t -> t -> t
--    setProp = set . prop2

--class (GetProp p t, SetProp p t) => HasProp2 p t where
--    prop2 :: p -> Lens' t (Prop p t)
--    default prop2 :: (GetProp p t, SetProp p t) => p -> Lens' t (Prop p t)
--    prop2 p = lens (getProp p) (flip $ setProp p)



--class GetAttrs p t where
--    getAttrs :: Proxy p -> t -> [Prop p t]
--    default getAttrs :: HasAttrs p t => Proxy p -> t -> [Prop p t]
--    getAttrs = view . attrs

--class SetAttrs p t where
--    setAttrs :: Proxy p -> [Prop p t] -> t -> t
--    default setAttrs :: HasAttrs p t => Proxy p -> [Prop p t] -> t -> t
--    setAttrs = set . attrs

--class (GetAttrs p t, SetAttrs p t) => HasAttrs p t where
--    attrs :: Proxy p -> Lens' t [Prop p t]
--    default attrs :: (GetAttrs p t, SetAttrs p t) => Proxy p -> Lens' t [Prop p t]
--    attrs p = lens (getAttrs p) (flip $ setAttrs p)




--class GetMems p t where getMems :: p -> t -> [Mem p t]

--instance {-# OVERLAPPABLE #-} GetAttrs p t                       => GetMems (Proxy p) t where getMems = getAttrs
--instance {-# OVERLAPPABLE #-} (GetProp p t, Mem p t ~ Prop p t) => GetMems p         t where getMems = getProp

--type family Mem p t where
--    Mem (Proxy p) t = Prop p t
--    Mem p         t = Prop p t



type HasProp   p t = (     Getter p t,      Setter p t)
type SubGetter p t = (Layered t, Getter p (Unlayered t), Prop p t ~ Prop p (Unlayered t))
type SubSetter p t = (Layered t, Setter p (Unlayered t), Prop p t ~ Prop p (Unlayered t))


-- === Utils === --

prop :: HasProp p t => p -> Lens' t (Prop p t)
prop p = lens (getter p) (flip $ setter p)


type a # p = Prop p a
--type a ## p = Attr p a

(#) :: Getter p t => t -> p -> t # p
(#) = flip getter

--(##) :: Getter2 sel p t => t -> Selector sel p -> Attr sel p t
--(##) = flip getter2



--(##) :: GetMems p t => t -> p -> [Mem p t]
--(##) = flip getMems

-- === Basic instances === --

type instance            Prop p (Cover t) = Prop p t
instance Getter p t => Getter p (Cover t) where getter p = getter p . view _Wrapped'
