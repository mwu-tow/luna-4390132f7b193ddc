module Data.Repr where

import Control.Lens


--------------------------
-- === Content show === --
--------------------------
-- | Utilities allowin easy implementation of show instances for structures
--   which provide content, so we can implement beginning and end sequence
--   independently from body

newtype Content a = Content a deriving (Functor, Traversable, Foldable)
makeWrapped ''Content

type ContentShow a = Show (Content a)
contentShow :: ContentShow a => a -> String
contentShow = show . Content ; {-# INLINE contentShow #-}
