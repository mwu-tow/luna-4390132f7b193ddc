module Data.Text.Position where

import Prologue hiding (range)
import Data.Bits
import Text.Printf
import Foreign.Storable
import Control.Monad.State.Dependent


-- FIXME[WD]: We should remove Delta and rename Offset -> Offset Linear or something like that
-------------------
-- === Delta === --
-------------------

-- | General text position difference descriptor.

-- === Definition === --

newtype Delta = Delta Int deriving (Bits, Bounded, Data, Enum, Eq, FiniteBits, Generic, Integral, Ix, NFData, Num, Ord, PrintfArg, Read, Real, Show, Storable)
makeWrapped ''Delta


-- === Instances === --

instance Convertible Int   Delta where convert = wrap
instance Convertible Delta Int   where convert = unwrap

instance Default   Delta where def    = 0
instance Mempty    Delta where mempty = 0
instance Semigroup Delta where (<>)   = (+)



--------------------
-- === Offset === --
--------------------

-- | File position as character offset from a control point (like the beginning of the file)

-- === Definition === --

newtype Offset = Offset Delta deriving (Bits, Bounded, Data, Default, Enum, Eq, FiniteBits, Integral, Ix, Mempty, Num, Ord, PrintfArg, Read, Real, Semigroup, Show, Storable)
makeLenses ''Offset


-- === Utils === --

succOffset :: MonadState Offset m => m ()
succOffset = incOffset 1

incOffset :: MonadState Offset m => Offset -> m ()
incOffset i = modify_ @Offset (+i)


-- === Instances === --

instance Convertible Delta  Offset where convert = wrap
instance Convertible Offset Delta  where convert = unwrap
instance Convertible Int    Offset where convert = convertVia @Delta
instance Convertible Offset Int    where convert = convertVia @Delta



---------------------
-- === Shifted === --
---------------------

data Shifted a = Shifted { _offset :: Delta
                         , __elem  :: a
                         } deriving (Show, Functor, Traversable, Foldable)
makeLenses ''Shifted

-- instance HasOffset (Shifted a) where offset = shifted_offset


----------------------
-- === Position === --
----------------------

-- | Line / column text position descriptor.

-- === Definition === --

data Position = Position { _line   :: !Delta
                         , _column :: !Delta
                         } deriving (Show, Eq, Data)
makeClassy ''Position


-- === Utils === --

getLine :: MonadGetter Position m => m Delta
getLine = view line <$> get @Position

getColumn :: MonadGetter Position m => m Delta
getColumn = view column <$> get @Position

modColumn :: MonadState Position m => (Delta -> Delta) -> m ()
modColumn f = modify_ @Position $ column %~ f

incColumn :: MonadState Position m => Delta -> m ()
incColumn = modColumn . (+)

succColumn :: MonadState Position m => m ()
succColumn = modColumn succ

succLine :: MonadState Position m => m ()
succLine = modify_ @Position $ (column .~ 0) . (line %~ succ)


-- === Instances === --

instance Mempty    Position where mempty                         = def
instance Default   Position where def                            = Position def def
instance Semigroup Position where Position l c <> Position l' c' = Position (l <> l') (c <> c')



-------------------
-- === Range === --
-------------------

-- === Definition === --

data Range = Range { _start :: !Delta
                   , _end   :: !Delta
                   } deriving (Show, Eq, Data)
makeClassy ''Range


-- === Instances === --

instance Default   Range where def                    = Range def def
instance Mempty    Range where mempty                 = Range mempty mempty
instance Semigroup Range where Range s _ <> Range _ e = Range s e



-----------------------
-- === FileOffset == --
-----------------------

-- === Definition === --

newtype FileOffset = FileOffset Delta deriving (Bits, Bounded, Data, Default, Enum, Eq, FiniteBits, Integral, Ix, Mempty, Num, Ord, PrintfArg, Read, Real, Semigroup, Show, Storable)
makeLenses ''FileOffset


-- === Instances === --

instance Convertible Delta FileOffset where convert = wrap
instance Convertible FileOffset Delta where convert = unwrap
instance Convertible Int   FileOffset where convert = convertVia @Delta
instance Convertible FileOffset Int   where convert = convertVia @Delta
