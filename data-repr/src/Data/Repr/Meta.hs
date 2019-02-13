{-# LANGUAGE RankNTypes #-}

module Data.Repr.Meta where

import Control.Lens


-- === Types ===

type family MetaRepr t a

class Meta t a where as' :: t -> Iso' a (MetaRepr t a)