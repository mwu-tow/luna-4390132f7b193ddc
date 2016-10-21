{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Type.Inference where

import           Prelude
import           Data.Typeable

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.Trans.Identity
import           Control.Monad.Primitive


-- === Definitions ===

-- | The Inferable type class describes a monad with a functional dependency on the given type.
--   It allows for writing polymorphic code and ensuring Haskell that the type will be resolved while
--   evaluating the monadic stack.

type    KnownType  cls t     = KnownTypeT cls t Identity
newtype KnownTypeT (cls :: *) (t :: k) (m :: * -> *) (a :: *) = KnownTypeT (IdentityT m a) deriving (Show, Functor, Monad, MonadIO, MonadFix, MonadTrans, Applicative, MonadThrow, MonadCatch, MonadMask, MonadPlus, Alternative)
makeWrapped ''KnownTypeT

class Monad m => Inferable cls t m | cls m -> t where infer_ :: cls -> t -> m () -- Depreciated

class Monad m => Inferable2 (cls :: *) (t :: k) m | cls m -> t where infer2_ :: m ()


-- === Utils === --

runInferenceT :: cls -> Proxy t -> KnownTypeT cls t m a -> m a
runInferenceT _ _ = runIdentityT . view _Wrapped' ; {-# INLINE runInferenceT #-}


runInferenceT2 :: forall cls t m a. KnownTypeT cls t m a -> m a
runInferenceT2 = runIdentityT . view _Wrapped' ; {-# INLINE runInferenceT2 #-}


runInference :: cls -> Proxy t -> KnownType cls t a -> a
runInference cls t m = runIdentity $ runInferenceT cls t m ; {-# INLINE runInference #-}

infer :: Inferable a t m => a -> t -> m t
infer a t = t <$ infer_ a t ; {-# INLINE infer #-}

inferM :: Inferable a t m => a -> m t -> m t
inferM a mt = do
    t <- mt
    infer_ a t
    return t
{-# INLINE inferM #-}

-- === Instances ===

-- Inferable recursive resolution
instance {-# OVERLAPPABLE #-} (t ~ t', Monad m)                              => Inferable cls t (KnownTypeT cls t' m) where infer_ _ _   = return ()           ; {-# INLINE infer_ #-}
instance {-# OVERLAPPABLE #-} (Inferable cls t n, MonadTrans m, Monad (m n)) => Inferable cls t (m n)                 where infer_ cls t = lift $ infer_ cls t ; {-# INLINE infer_ #-}

-- Primitive
instance PrimMonad m => PrimMonad (KnownTypeT cls t m) where
    type PrimState (KnownTypeT cls t m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}


-- Inferable recursive resolution
instance {-# OVERLAPPABLE #-} (t ~ t', Monad m)                               => Inferable2 cls t (KnownTypeT cls t' m) where infer2_ = return ()              ; {-# INLINE infer2_ #-}
instance {-# OVERLAPPABLE #-} (Inferable2 cls t n, MonadTrans m, Monad (m n)) => Inferable2 cls t (m n)                 where infer2_ = lift $ infer2_ @cls @t ; {-# INLINE infer2_ #-}
