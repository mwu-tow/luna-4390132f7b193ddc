{-# LANGUAGE UndecidableInstances #-}

module Data.Layer_OLD.Immersed where


import Prelude

import Control.Lens
import Control.Monad
import Data.Layer_OLD



-- === Immersed layers ===

-- Non-monadic interface

class                                                                  Immersed l a where immersed :: Lens' l a
instance {-# OVERLAPPABLE #-}                                          Immersed l l where immersed = id
instance {-# OVERLAPPABLE #-} (Layered l, Immersed (Unlayered l) a) => Immersed l a where immersed = layered . immersed

-- Monadic interface

class                                                                                 ImmersedM l m a where viewImmersedM :: l -> m a
instance {-# OVERLAPPABLE #-} (Monad m)                                            => ImmersedM l m l where viewImmersedM = return
instance {-# OVERLAPPABLE #-} (Monad m, LayeredM m l, ImmersedM (Unlayered l) m a) => ImmersedM l m a where viewImmersedM = viewLayeredM >=> viewImmersedM
