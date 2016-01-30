module Data.Coat where

import Prelude
import Control.Lens
import Control.Monad
import Data.Layer


-- Non-monadic interface

class Coated c where
    coated :: Lens (c a) (c a') a a'
    default coated :: (Unwrapped (c a) ~ a, Unwrapped (c a') ~ a', Rewrapping (c a) (c a')) => Lens (c a) (c a') a a'
    coated = _Wrapped


class IsCoat c where
    coat :: a -> c a
    default coat :: (IsLayer (c a), Unlayered (c a) ~ a) => a -> c a
    coat = layer

uncoat :: Coated c => c a -> a
uncoat = view coated


-- Monadic interface

class CoatedM m c where
    viewCoatedM :: c a -> m a
    setCoatedM  :: a -> c a -> m (c a)


uncoatM :: CoatedM m c => c a -> m a
uncoatM = viewCoatedM

withCoatedM :: (CoatedM m c, Monad m) => (a -> m a) -> c a -> m (c a)
withCoatedM f l = viewCoatedM l >>= f >>= flip setCoatedM l

withCoatedM' :: (CoatedM m c, Monad m) => (a -> a) -> c a -> m (c a)
withCoatedM' = withCoatedM . (return .)


class CoatConstructor a m c where constructCoat :: a -> m (c a)
class CoatGenerator     m c where generateCoat  :: a -> m (c a)

