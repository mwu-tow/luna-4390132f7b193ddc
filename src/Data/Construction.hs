{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Construction where

import Prelude hiding (break)


-- Construction

type family Deconstructed a

class            Maker           a where make        :: Deconstructed a -> a
class            Breaker         a where break       :: a -> Deconstructed a
class Monad m => Constructor   m a where construct   :: Deconstructed a -> m a   ; default construct :: Creator m a => Deconstructed a -> m a
                                                                                 ; construct _ = create ; {-# INLINE construct #-}
class Monad m => Deconstructor m a where deconstruct :: a -> m (Deconstructed a) ; default deconstruct :: Breaker a => a -> m (Deconstructed a)
                                                                                 ; deconstruct = return . break ; {-# INLINE deconstruct #-}
class Monad m => Creator       m a where create      :: m a                      ; default create :: Maker a => Deconstructed a -> m a
                                                                                 ; create = return . make ; {-# INLINE create #-}
class Monad m => Generator     m a where new         :: m a                      ; default new :: Maker a => Deconstructed a -> m a
                                                                                 ; new = return . make ; {-# INLINE new #-}
class Monad m => Destructor    m a where destruct    :: a -> m ()                ; default destruct :: Deconstructor m a => a -> m ()
                                                                                 ; destruct = deconstruct_ ; {-# INLINE destruct #-}

deconstruct_ :: Deconstructor m a => a -> m ()
deconstruct_ a = () <$ deconstruct a ; {-# INLINE deconstruct_ #-}

-- Producers & Consumers

class Monad m => Producer m a where produce :: m a
class Monad m => Consumer m a where consume :: a -> m ()

-- Registration

class Monad m => Register   m a where register   :: a -> m ()
class Monad m => Unregister m a where unregister :: a -> m ()
