{-# LANGUAGE UndecidableInstances #-}

module Data.Layer.Coat where

import Prelude
import Control.Lens
import Control.Monad
import Data.Construction
import Data.Layer


-- === Coat ===

newtype Coat a = Coat a deriving (Show, Functor, Foldable, Traversable)
type instance Unlayered (Coat a) = a

type family Uncoated a where Uncoated (Coat a) = a
                             Uncoated a        = Uncoated (Unlayered a)

-- Instances

instance Layered   (Coat a)
instance Rewrapped (Coat a) (Coat a')
instance Wrapped   (Coat a) where
    type Unwrapped (Coat a) = a
    _Wrapped' = iso (\(Coat a) -> a) Coat



-- === Coated ===

-- pure interface

class Coated a where coated :: Lens' a (Uncoated a)
instance {-# OVERLAPPABLE #-} ( Layered a
                              , Coated (Unlayered a)
                              , Uncoated a ~ Uncoated (Unlayered a)
                              ) => Coated a        where coated = layered . coated
instance {-# OVERLAPPABLE #-}      Coated (Coat a) where coated = _Wrapped'

uncoat :: Coated a => a -> Uncoated a
uncoat = view coated


-- monadic interface

class CoatedM m a where viewCoatedM :: a -> m (Uncoated a)
                        setCoatedM  :: Uncoated a -> a -> m a

instance {-# OVERLAPPABLE #-} ( Monad m
                              , LayeredM m a
                              , CoatedM m (Unlayered a)
                              , Uncoated a ~ Uncoated (Unlayered a)
                              )       => CoatedM m a        where viewCoatedM = viewLayeredM >=> viewCoatedM
                                                                  setCoatedM  = withLayeredM . setCoatedM
instance {-# OVERLAPPABLE #-} Monad m => CoatedM m (Coat a) where viewCoatedM   = return . uncoat
                                                                  setCoatedM  v = return . set coated v


-- === Coat generator ===

class CoatConstructor m a where constructCoat :: Uncoated a -> m a
instance {-# OVERLAPPABLE #-} ( Monad m
                              , CoatConstructor m (Destructed a)
                              , Uncoated a ~ Uncoated (Destructed a)
                              , Constructor m a
                              )       => CoatConstructor m a        where constructCoat = constructCoat >=> construct
instance {-# OVERLAPPABLE #-} Monad m => CoatConstructor m (Coat a) where constructCoat = return . Coat


--class Destruction m a where destroy :: a -> m (Uncoated a)
--instance {-# OVERLAPPABLE #-} Monad m => Destruction m (Coat a) where destroy = return . unlayer
--instance {-# OVERLAPPABLE #-} Monad m => Destruction m (Coat a) where destroy = return . unlayer

--type family Deconstructed a
--class Construction   m a where construct   :: Deconstructed a -> m a
--class Deconstruction m a where deconstruct :: a               -> m (Deconstructed a)