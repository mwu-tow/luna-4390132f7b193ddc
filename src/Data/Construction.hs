module Data.Construction where



-- Constructors & Destructors

type family Destructed a

class Monad m => Constructor m a where construct :: Destructed a -> m a
class Monad m => Destructor  m a where destruct  :: a -> m (Destructed a)

-- Producers & Consumers

class Monad m => Producer m a where produce :: m a
class Monad m => Consumer m a where consume :: a -> m ()

-- Make & Destroy

class Monad m => Maker     m a where make    :: m a
class Monad m => Destroyer m a where destroy :: a -> m ()

-- Registration

class Monad m => Register   m a where register   :: a -> m ()
class Monad m => Unregister m a where unregister :: a -> m ()








-- 
-- 
-- 
-- 
-- -- Construction
-- 
-- type family Deconstructed a
-- 
-- class Monad m => Constructor   m a where construct   :: Deconstructed a -> m a
-- class Monad m => Deconstructor m a where deconstruct :: a -> m (Deconstructed a)
-- class Monad m => Creator       m a where create      :: m a
-- class Monad m => Destructor    m a where destruct    :: a -> m ()
--                                          default destruct :: Deconstructor m a => a -> m ()
--                                          destruct = deconstruct_ ; {-# INLINE destruct #-}
-- 
-- construct_ :: Constructor m a => Deconstructed a -> m ()
-- construct_ a = () <$ construct a ; {-# INLINE construct_ #-}
-- 
-- deconstruct_ :: Deconstructor m a => a -> m ()
-- deconstruct_ a = () <$ deconstruct a ; {-# INLINE deconstruct_ #-}
-- 
-- -- Producers & Consumers
-- 
-- class Monad m => Producer m a where produce :: m a
-- class Monad m => Consumer m a where consume :: a -> m ()
-- 
-- -- Registration
-- 
-- class Monad m => Register   m a where register   :: a -> m ()
-- class Monad m => Unregister m a where unregister :: a -> m ()