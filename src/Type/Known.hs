{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash           #-}
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

class Known (t :: k) where fromType :: KnownTypeVal k

type family KnownTypeVal (t :: k) :: Type where
    KnownTypeVal Nat    = Integer
    KnownTypeVal Symbol = String
    KnownTypeVal t      = t


-- === Instances === --

instance KnownNat    t => Known (t :: Nat)    where fromType = natVal    (Proxy @t)
instance KnownSymbol t => Known (t :: Symbol) where fromType = symbolVal (Proxy @t)
