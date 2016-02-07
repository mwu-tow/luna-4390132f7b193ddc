{-# LANGUAGE UndecidableInstances #-}

module Type.Inference where

import Prelude
import Control.Lens
import Data.Typeable

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Fix


-- === Definitions ===

-- | The Inferable type class describes a monad with a functional dependency on the given type.
--   It allows for writing polymorphic code and ensuring Haskell that the type will be resolved while
--   evaluating the monadic stack.

type    KnownType  cls t     = KnownTypeT cls t Identity
newtype KnownTypeT cls t m a = KnownTypeT (m a) deriving (Show, Functor)
makeWrapped ''KnownTypeT

class Monad m => Inferable cls t m | cls m -> t where infer_ :: cls -> t -> m ()


-- === Utils === --

runInferenceT :: cls -> Proxy t -> KnownTypeT cls t m a -> m a
runInferenceT _ _ = view _Wrapped'

runInference :: cls -> Proxy t -> KnownType cls t a -> a
runInference cls t m = runIdentity $ runInferenceT cls t m

infer :: Inferable a t m => a -> t -> m t
infer a t = t <$ infer_ a t

inferM :: Inferable a t m => a -> m t -> m t
inferM a t = t <* (infer_ a =<< t)


-- === Instances === ---

instance Applicative m => Applicative (KnownTypeT cls t m) where pure                            = KnownTypeT . pure                             ; {-# INLINE pure   #-}
                                                                 KnownTypeT mf <*> KnownTypeT ma = KnownTypeT $ mf <*> ma                        ; {-# INLINE (<*>)  #-}
instance Monad m       => Monad       (KnownTypeT cls t m) where (KnownTypeT ma) >>= f           = KnownTypeT $ ma >>= (fmap (view _Wrapped') f) ; {-# INLINE (>>=)  #-}
instance MonadIO  m    => MonadIO     (KnownTypeT cls t m) where liftIO                          = KnownTypeT . liftIO                           ; {-# INLINE liftIO #-}
instance MonadFix m    => MonadFix    (KnownTypeT cls t m) where mfix f                          = KnownTypeT $ mfix $ view _Wrapped' <$> f      ; {-# INLINE mfix   #-}
instance                  MonadTrans  (KnownTypeT cls t)   where lift                            = KnownTypeT                                    ; {-# INLINE lift   #-}


-- Inferable recursive resolution

instance {-# OVERLAPPABLE #-} (t ~ t', Monad m)                              => Inferable cls t (KnownTypeT cls t' m) where infer_ _ _   = return ()           ; {-# INLINE infer_ #-}
instance {-# OVERLAPPABLE #-} (Inferable cls t n, MonadTrans m, Monad (m n)) => Inferable cls t (m n)                 where infer_ cls t = lift $ infer_ cls t ; {-# INLINE infer_ #-}
