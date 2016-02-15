{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo          #-}

module Data.Layer.Cover where

import Prelude
import Control.Lens      hiding (Getter, Setter)
import Control.Monad
import Control.Monad.Fix
import Data.Construction
import Data.Convert
import Data.Layer

-- === Cover ===

newtype Cover a = Cover a deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
type instance Unlayered (Cover a) = a

--type family Uncovered a where Uncovered (Cover a) = a
--                              Uncovered a         = Uncovered (Unlayered a)

type family   Uncovered a
type instance Uncovered (Cover a) = a
-- TODO[WD]: Add TH function to automatically create the default Uncovered instance:
--           type instance Uncovered <a> = Uncovered (Unlayered <a>)

-- Instances

instance Layered   (Cover a)
instance Rewrapped (Cover a) (Cover a')
instance Wrapped   (Cover a) where
    type Unwrapped (Cover a) = a
    _Wrapped' = iso (\(Cover a) -> a) Cover

instance Castable    a a' => Castable    (Cover a) (Cover a') where cast    = _Wrapped %~ cast
instance Convertible a a' => Convertible (Cover a) (Cover a') where convert = _Wrapped %~ convert


-- === Covered ===

-- pure interface

class Covered a where covered :: Lens' a (Uncovered a) -- TODO[WD]: refactor to covered'
class FullCovered a a' where fullCovered :: Lens a a' (Uncovered a) (Uncovered a') -- TODO[WD]: refactor to covered
instance {-# OVERLAPPABLE #-} ( Layered a
                              , Covered (Unlayered a)
                              , Uncovered a ~ Uncovered (Unlayered a)
                              ) => Covered a         where covered = layered . covered
instance {-# OVERLAPPABLE #-}      Covered (Cover a) where covered = _Wrapped'

uncover :: Covered a => a -> Uncovered a
uncover = view covered


-- monadic interface

class CoveredM m a where viewCoveredM :: a -> m (Uncovered a)
                         setCoveredM  :: Uncovered a -> a -> m a

instance {-# OVERLAPPABLE #-} ( Monad m
                              , LayeredM m a
                              , CoveredM m (Unlayered a)
                              , Uncovered a ~ Uncovered (Unlayered a)
                              )       => CoveredM m a         where viewCoveredM   = viewLayeredM >=> viewCoveredM
                                                                    setCoveredM    = withLayeredM . setCoveredM
instance {-# OVERLAPPABLE #-} Monad m => CoveredM m (Cover a) where viewCoveredM   = return . uncover
                                                                    setCoveredM  v = return . set covered v


-- === Cover generator ===

class Monad m => CoverConstructor    m a where constructCover    :: Uncovered a -> m a
class Monad m => CoverConstructorFix m a where constructCoverFix :: Uncovered a -> m a
class Monad m => CoverDestructor     m a where destructCover     :: a -> m (Uncovered a)
class Monad m => CoverDestructorFix  m a where destructCoverFix  :: a -> m (Uncovered a)

instance {-# OVERLAPPABLE #-} ( CoverConstructor m (Unlayered a)
                              , Uncovered a ~ Uncovered (Unlayered a)
                              , LayerConstructor m a
                              , Monad m ) => CoverConstructor m a         where constructCover = constructCover >=> constructLayer
instance {-# OVERLAPPABLE #-}   Monad m   => CoverConstructor m (Cover a) where constructCover = return . Cover

instance {-# OVERLAPPABLE #-} ( CoverConstructorFix m (Unlayered a)
                              , Uncovered a ~ Uncovered (Unlayered a)
                              , LayerConstructor m a
                              , MonadFix m ) => CoverConstructorFix m a         where
    constructCoverFix base = mdo
        out <- constructLayer l
        l   <- constructCoverFix base
        return out
instance {-# OVERLAPPABLE #-}   Monad m   => CoverConstructorFix m (Cover a) where constructCoverFix = return . Cover


instance {-# OVERLAPPABLE #-} ( CoverDestructor m (Unlayered a)
                              , Uncovered a ~ Uncovered (Unlayered a)
                              , LayerDestructor m a) => CoverDestructor m a         where destructCover = destructLayer >=> destructCover

instance Monad m => CoverDestructor m (Cover a) where destructCover = return . view _Wrapped'
--instance {-# OVERLAPPABLE #-}   Monad m   => CoverDestructor m (Cover a) where destructCover = return . Cover


--class CoverConstructor m a where constructCover :: Uncovered a -> m a
--instance {-# OVERLAPPABLE #-} ( CoverConstructor m (Destructed a)
--                              , Uncovered a ~ Uncovered (Destructed a)
--                              , Constructor m a
--                              , Monad m ) => CoverConstructor m a         where constructCover = constructCover >=> construct
--instance {-# OVERLAPPABLE #-}   Monad m   => CoverConstructor m (Cover a) where constructCover = return . Cover

--class CoverDestructor m a where destructCover :: a -> m (Uncovered a)
--instance {-# OVERLAPPABLE #-} ( Uncovered a ~ Uncovered (Destructed a)
--                              , CoverDestructor m (Destructed a)
--                              , Destructor m a
--                              , Monad m ) => CoverDestructor m a         where destructCover = destruct >=> destructCover
--instance {-# OVERLAPPABLE #-}   Monad m   => CoverDestructor m (Cover a) where destructCover = return . view _Wrapped'

--class Destruction m a where destroy :: a -> m (Uncovered a)
--instance {-# OVERLAPPABLE #-} Monad m => Destruction m (Cover a) where destroy = return . unlayer
--instance {-# OVERLAPPABLE #-} Monad m => Destruction m (Cover a) where destroy = return . unlayer

--type family Deconstructed a
--class Construction   m a where construct   :: Deconstructed a -> m a
--class Deconstruction m a where deconstruct :: a               -> m (Deconstructed a)
-- >//>
-- >\\>
-- >><>


-- Attributes

--instance MayHaveAttr a (Cover t) where checkAttr _ = Nothing
