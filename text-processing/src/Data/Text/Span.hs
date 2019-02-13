 {-# LANGUAGE UndecidableInstances #-}
 {-# EXT      InlineAll            #-}

module Data.Text.Span where

import qualified Control.Monad.State.Layered as State
-- import qualified Foreign.Storable.Deriving   as Storable

import Prologue            hiding (length)
import Data.Maybe          (isJust)
import Data.Text.Position (Delta)

----------------------------
-- === LeftSpacedSpan === --
----------------------------

-- === Definition === --

data SpacedSpan = SpacedSpan 
    { __offset :: !Delta
    , __length :: !Delta 
    } deriving (Eq, Ord, Show)
-- Storable.derive ''SpacedSpan

newtype LeftSpacedSpan  = LeftSpacedSpan  SpacedSpan deriving (Eq, Ord, Show)
newtype RightSpacedSpan = RightSpacedSpan SpacedSpan deriving (Eq, Ord, Show)
makeLenses ''SpacedSpan
makeLenses ''LeftSpacedSpan
makeLenses ''RightSpacedSpan

class    IsSpacedSpan t               where spacedSpan :: Iso' t SpacedSpan
instance IsSpacedSpan LeftSpacedSpan  where spacedSpan = wrapped
instance IsSpacedSpan RightSpacedSpan where spacedSpan = wrapped


-- === Construction === --

leftSpacedSpan  :: Delta -> Delta -> LeftSpacedSpan
rightSpacedSpan :: Delta -> Delta -> RightSpacedSpan
leftSpacedSpan  = LeftSpacedSpan  .: SpacedSpan ; {-# INLINE leftSpacedSpan #-}
rightSpacedSpan = RightSpacedSpan .: SpacedSpan ; {-# INLINE rightSpacedSpan #-}


-- === Props === --

offset, length :: IsSpacedSpan t => Lens' t Delta
offset = spacedSpan . spacedSpan_offset ; {-# INLINE offset #-}
length = spacedSpan . spacedSpan_length ; {-# INLINE length #-}

asOffsetSpan :: IsSpacedSpan t => t -> t
asOffsetSpan s = s & offset %~ (+ s ^. length)
                   & length .~ 0
{-# INLINE asOffsetSpan #-}

measure :: IsSpacedSpan t => t -> Delta
measure t = t ^. offset + t ^. length ; {-# INLINE measure #-}

concat :: Num s => LeftSpacedSpan -> LeftSpacedSpan -> LeftSpacedSpan
concat (unwrap -> SpacedSpan off s) (unwrap -> SpacedSpan off' s') 
    = wrap $ SpacedSpan off (s + off' + s')


-- === Instances === --

instance Num s => Default SpacedSpan where def    = SpacedSpan 0 0
instance Num s => Mempty  SpacedSpan where mempty = SpacedSpan 0 0

deriving instance Num s => Default LeftSpacedSpan
deriving instance Num s => Default RightSpacedSpan
deriving instance Num s => Mempty  LeftSpacedSpan
deriving instance Num s => Mempty  RightSpacedSpan

instance (Num s, Ord s) => Semigroup LeftSpacedSpan where
    (unwrap -> SpacedSpan off s) <> (unwrap -> SpacedSpan off' s') 
        = if s > 0 then leftSpacedSpan off (s + off' + s')
                   else leftSpacedSpan (off + off') s'

instance (Num s, Ord s) => Semigroup RightSpacedSpan where
    (unwrap -> SpacedSpan s off) <> (unwrap -> SpacedSpan s' off')
        = if s' > 0 then rightSpacedSpan (s + off + s') off'
                    else rightSpacedSpan s (off + off')

-- Conversions
instance Convertible SpacedSpan      (Delta,Delta)   where convert (SpacedSpan o l) = (o,l)
instance Convertible (Delta,Delta)   SpacedSpan      where convert (o,l) = (SpacedSpan o l)
instance Convertible SpacedSpan      LeftSpacedSpan  where convert = wrap
instance Convertible SpacedSpan      RightSpacedSpan where convert = wrap
instance Convertible LeftSpacedSpan  SpacedSpan      where convert = unwrap
instance Convertible RightSpacedSpan SpacedSpan      where convert = unwrap
instance Convertible LeftSpacedSpan  (Delta,Delta)   where convert = convertVia @SpacedSpan
instance Convertible RightSpacedSpan (Delta,Delta)   where convert = convertVia @SpacedSpan
instance Convertible (Delta,Delta)   LeftSpacedSpan  where convert = convertVia @SpacedSpan
instance Convertible (Delta,Delta)   RightSpacedSpan where convert = convertVia @SpacedSpan
