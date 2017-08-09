{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

module Type.Known where

import Prelude
import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits


----------------------------------------
-- === Types to values conversion === --
----------------------------------------

-- === Definition === --

class KnownType (t :: k) where fromType :: KnownTypeVal k

type family KnownTypeVal (t :: k) :: Type where
    KnownTypeVal Nat    = Integer
    KnownTypeVal Symbol = String
    KnownTypeVal t      = t


-- === Instances === --

instance KnownNat    t => KnownType (t :: Nat)    where fromType = natVal    (Proxy @t) ; {-# INLINE fromType #-}
instance KnownSymbol t => KnownType (t :: Symbol) where fromType = symbolVal (Proxy @t) ; {-# INLINE fromType #-}
