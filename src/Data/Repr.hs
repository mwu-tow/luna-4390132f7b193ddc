
{-# LANGUAGE UndecidableInstances #-}

module Data.Repr  where

import Prelude



-- == Types ==

class Repr a where
    repr :: a -> String


-- === Instances ===

instance {-# OVERLAPPABLE #-} Show a => Repr a where
    repr = show

