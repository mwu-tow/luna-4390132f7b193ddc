module Data.Layer_OLD where

import Prelude
import Control.Lens
import Control.Monad

--------------------
-- === Layers === --
--------------------
-- | Layer is a generalization of Wrapper able to carry some information, so it is a lens, not an isomorphism.


-- === Definitions === --

type family Unlayered l

-- Non-monadic interface

class Layered l where
    layered :: Lens' l (Unlayered l)
    default layered :: (Unlayered l ~ Unwrapped l, Wrapped l) => Lens' l (Unlayered l)
    layered = _Wrapped'

class TransLayered l l' where
    transLayered :: Lens l l' (Unlayered l) (Unlayered l')
    default transLayered :: (Unlayered l ~ Unwrapped l, Unlayered l' ~ Unwrapped l', Rewrapping l l') => Lens l l' (Unlayered l) (Unlayered l')
    transLayered = _Wrapped ; {-# INLINE transLayered #-}

unlayer :: Layered l => l -> Unlayered l
unlayer = view layered


-- Monadic interface

class LayeredM m l where
    viewLayeredM         ::                         l -> m (Unlayered l)
    default viewLayeredM :: (Layered l, Monad m) => l -> m (Unlayered l)
    viewLayeredM = return . unlayer

    setLayeredM         ::                         Unlayered l -> l -> m l
    default setLayeredM :: (Layered l, Monad m) => Unlayered l -> l -> m l
    setLayeredM = (fmap . fmap) return $ set layered


unlayerM :: LayeredM m l => l -> m (Unlayered l)
unlayerM = viewLayeredM

withLayeredM :: (LayeredM m a, Monad m) => (Unlayered a -> m (Unlayered a)) -> a -> m a
withLayeredM f l = viewLayeredM l >>= f >>= flip setLayeredM l

withLayeredM' :: (LayeredM m a, Monad m) => (Unlayered a -> Unlayered a) -> a -> m a
withLayeredM' = withLayeredM . (return .)


-- === Constructors === --

class Monad m => LayerConstructor m l where constructLayer :: (Unlayered l) -> m l
class Monad m => LayerDestructor  m l where destructLayer  :: l -> m (Unlayered l)
