module Data.Layer where

import Prelude
import Control.Lens
import Control.Monad


-- === Data Layering ===

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

class IsLayer l where
    layer :: Unlayered l -> l

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



class Monad m => LayerConstructor m l where constructLayer :: (Unlayered l) -> m l
class Monad m => LayerDestructor  m l where destructLayer  :: l -> m (Unlayered l)


--class Cover   m l where cover   :: Unlayered l -> m l
--class Uncover m l where uncover :: l -> m (Unlayered l)