{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Type.Inference where

import Prelude
import Data.Typeable

import Control.Applicative
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Monad.Primitive
import GHC.Exts (Constraint)


-- === Definitions ===

-- | The Inferable type class describes a monad with a functional dependency on the given type.
--   It allows for writing polymorphic code and ensuring Haskell that the type will be resolved while
--   evaluating the monadic stack.

type    KnownType  cls t = KnownTypeT cls t Identity
newtype KnownTypeT (cls :: ck) (t :: tk) (m :: * -> *) (a :: *) = KnownTypeT (IdentityT m a) deriving (Show, Functor, Monad, MonadIO, MonadFix, MonadTrans, Applicative, MonadThrow, MonadCatch, MonadMask, MonadPlus, Alternative)
makeWrapped ''KnownTypeT

type family Infer (p :: k) m where
    Infer p (KnownTypeT p t m) = t
    Infer p (t m)              = Infer p m

type family TryInfer (p :: k) m a :: Constraint where
    TryInfer p (KnownTypeT p t m) a = (a ~ t)
    TryInfer p (t m)              a = TryInfer p m a
    TryInfer p m                  a = ()


-- === Utils === --

inferT :: forall cls t m a. KnownTypeT cls t m a -> m a
inferT = runIdentityT . view _Wrapped'


-- === Instances ===

-- Primitive
instance PrimMonad m => PrimMonad (KnownTypeT cls t m) where
    type PrimState (KnownTypeT cls t m) = PrimState m
    primitive = lift . primitive
