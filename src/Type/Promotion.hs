{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}

module Type.Promotion where

import Prelude

import Data.Typeable
import GHC.TypeLits


-- === Promotion classes ===

class Known (t :: k) (val :: *) where typeVal :: Proxy t -> val
--type family TypeVal (t :: k) :: *
--type Known' t = Known t (TypeVal t)



-- === Basic instances ===

-- Nat
instance (Num i, KnownNat t) => Known (t :: Nat) i where typeVal = fromIntegral . natVal

-- Bool
--type instance TypeVal (k :: Bool) = Bool
instance val ~ Bool => Known True  val where typeVal _ = True
instance val ~ Bool => Known False val where typeVal _ = False

-- Maybe a
--type instance TypeVal ('Just a) = Maybe (TypeVal a)
instance val ~ Maybe a              => Known 'Nothing  val where typeVal _ = Nothing
instance (val ~ Maybe a, Known t a) => Known ('Just t) val where typeVal _ = Just $ typeVal (Proxy :: Proxy t)

-- Either l r
instance (val ~ Either l r, Known t l) => Known ('Left  t) val where typeVal _ = Left  $ typeVal (Proxy :: Proxy t)
instance (val ~ Either l r, Known t r) => Known ('Right t) val where typeVal _ = Right $ typeVal (Proxy :: Proxy t)

-- Lists
instance Known ('[]) [a] where typeVal _ = []
instance (Known t a, Known ts [a]) => Known (t ': ts) [a] where typeVal _ = typeVal (Proxy :: Proxy t) : typeVal (Proxy :: Proxy ts)

-- KnownNats
class                                  KnownNats (nats :: [Nat]) where natVals :: Proxy nats -> [Integer]
instance                               KnownNats '[]             where natVals _ = []                                                      ; {-# INLINE natVals #-}
instance (KnownNat n, KnownNats ns) => KnownNats (n ': ns)       where natVals _ = natVal (Proxy :: Proxy n) : natVals (Proxy :: Proxy ns) ; {-# INLINE natVals #-}
