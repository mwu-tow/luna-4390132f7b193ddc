{-# LANGUAGE Strict #-}
{-# LANGUAGE AutoDeriveTypeable #-}


module Control.Monad.State.Strict2 where

import Prelude

import Control.Monad.State.Class

newtype State s a = State { runState :: s -> (a,s) }

get' ::      State s s
put' :: s -> State s ()
get'   = State $ \s -> (s ,s) ; {-# INLINE get' #-}
put' a = State $ \_ -> ((),a) ; {-# INLINE put' #-}

instance Functor (State s) where
    fmap f st = State $ \s -> let (a, s') = runState st s in (f a, s') ; {-# INLINE fmap #-}

instance Applicative (State s) where
    pure a = State $ \s -> (a,s) ; {-# INLINE pure #-}
    stf <*> sta = State $ \s -> let (f,s') = runState stf s; (a,s'') = runState sta s' in (f a, s'') ; {-# INLINE (<*>) #-}

instance Monad (State s) where
    return = pure ; {-# INLINE return #-}
    sta >>= f = State $ \s -> let (a,s') = runState sta s in runState (f a) s' ; {-# INLINE (>>=) #-}
