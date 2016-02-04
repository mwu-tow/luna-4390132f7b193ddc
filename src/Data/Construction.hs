{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Construction where


 -- Construction
 
 type family Deconstructed a
 
 class Monad m => Creator       m a where create      ::                    m a
 class Monad m => Constructor   m a where construct   :: Deconstructed a -> m a   ; default construct :: Creator m a => Deconstructed a -> m a
                                                                                  ; construct _ = create ; {-# INLINE construct #-}
 class Monad m => Deconstructor m a where deconstruct :: a -> m (Deconstructed a)
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