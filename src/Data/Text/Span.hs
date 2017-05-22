 {-# LANGUAGE UndecidableInstances #-}
 {-# EXT      InlineAll            #-}

module Data.Text.Span where

import Prologue            hiding (length)
import Data.Maybe          (isJust)
import Control.Monad.State.Dependent


----------------------------
-- === LeftSpacedSpan === --
----------------------------

-- === Definition === --

data    SpacedSpan      s = SpacedSpan      { __offset :: !s, __length :: !s } deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
newtype LeftSpacedSpan  s = LeftSpacedSpan  (SpacedSpan s) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
newtype RightSpacedSpan s = RightSpacedSpan (SpacedSpan s) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
makeLenses ''SpacedSpan
makeLenses ''LeftSpacedSpan
makeLenses ''RightSpacedSpan

class    IsSpacedSpan t               where spacedSpan :: forall s. Iso' (t s) (SpacedSpan s)
instance IsSpacedSpan LeftSpacedSpan  where spacedSpan = wrapped
instance IsSpacedSpan RightSpacedSpan where spacedSpan = wrapped


-- === Construction === --

leftSpacedSpan :: s -> s -> LeftSpacedSpan s
leftSpacedSpan = LeftSpacedSpan .: SpacedSpan

rightSpacedSpan :: s -> s -> RightSpacedSpan s
rightSpacedSpan = RightSpacedSpan .: SpacedSpan


-- === Props === --

offset, length :: IsSpacedSpan t => Lens' (t s) s
offset = spacedSpan . spacedSpan_offset
length = spacedSpan . spacedSpan_length

asOffsetSpan :: (IsSpacedSpan t, Num s) => t s -> t s
asOffsetSpan s = s & offset %~ (+ s ^. length)
                   & length .~ 0


-- === Instances === --

instance Num s => Default (SpacedSpan s) where def    = SpacedSpan 0 0
instance Num s => Mempty  (SpacedSpan s) where mempty = SpacedSpan 0 0

deriving instance Num s => Default (LeftSpacedSpan  s)
deriving instance Num s => Default (RightSpacedSpan s)
deriving instance Num s => Mempty  (LeftSpacedSpan  s)
deriving instance Num s => Mempty  (RightSpacedSpan s)

instance (Num s, Ord s) => Semigroup (LeftSpacedSpan s) where
    (unwrap -> SpacedSpan off s) <> (unwrap -> SpacedSpan off' s') = if s > 0 then leftSpacedSpan off (s + off' + s')
                                                                              else leftSpacedSpan (off + off') s'

instance (Num s, Ord s) => Semigroup (RightSpacedSpan s) where
    (unwrap -> SpacedSpan s off) <> (unwrap -> SpacedSpan s' off') = if s' > 0 then rightSpacedSpan (s + off + s') off'
                                                                               else rightSpacedSpan s (off + off')

-- Conversions
instance Convertible (SpacedSpan s)      (s,s)               where convert (SpacedSpan o l) = (o,l)
instance Convertible (s,s)               (SpacedSpan s)      where convert (o,l) = (SpacedSpan o l)
instance Convertible (SpacedSpan s)      (LeftSpacedSpan  s) where convert = wrap
instance Convertible (SpacedSpan s)      (RightSpacedSpan s) where convert = wrap
instance Convertible (LeftSpacedSpan  s) (SpacedSpan s)      where convert = unwrap
instance Convertible (RightSpacedSpan s) (SpacedSpan s)      where convert = unwrap
instance Convertible (LeftSpacedSpan  s) (s,s)               where convert = convertVia @(SpacedSpan s)
instance Convertible (RightSpacedSpan s) (s,s)               where convert = convertVia @(SpacedSpan s)
instance Convertible (s,s)               (LeftSpacedSpan  s) where convert = convertVia @(SpacedSpan s)
instance Convertible (s,s)               (RightSpacedSpan s) where convert = convertVia @(SpacedSpan s)
