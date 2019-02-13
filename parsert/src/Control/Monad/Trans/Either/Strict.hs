{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Strict #-}

module Control.Monad.Trans.Either.Strict where

import Prelude hiding ((.), Either, Left, Right, either)
import Control.Applicative
import Control.Monad (liftM, MonadPlus(..))
import Control.Monad.Base (MonadBase(..), liftBaseDefault)
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Free.Class
import Control.Monad.Catch as MonadCatch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State (MonadState,get,put)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith, defaultRestoreM)
import Control.Monad.Writer.Class
import Control.Monad.Random (MonadRandom,getRandom,getRandoms,getRandomR,getRandomRs)
import Control.Monad.Morph (MFunctor, hoist)
-- import Data.Either.Combinators ( swapEither )
import Data.Either.Strict

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) f g = \x -> f (g x) ; {-# INLINE (.) #-}
--

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Monad m => Functor (EitherT e m) where
  fmap f = \a -> EitherT $ liftM (fmap f) $ runEitherT a ; {-# INLINE fmap #-}

instance Monad m => Applicative (EitherT e m) where
  pure a  = EitherT $ return (Right a)
  {-# INLINE pure #-}
  EitherT f <*> EitherT v = EitherT $ f >>= \mf -> case mf of
	Left  e -> return (Left e)
	Right k -> v >>= \mv -> case mv of
	  Left  e -> return (Left e)
	  Right x -> return (Right (k x))
  {-# INLINE (<*>) #-}

instance Monad m => Monad (EitherT e m) where
  return a = EitherT $ return (Right a) ; {-# INLINE return #-}
  m >>= k  = EitherT $ do
	a <- runEitherT m
	case a of
	  Left  l -> return (Left l)
	  Right r -> runEitherT (k r)
  {-# INLINE (>>=) #-}
  fail a = EitherT $ fail a
  {-# INLINE fail #-}

instance MonadTrans (EitherT e) where
  lift a = EitherT $ liftM Right a
  {-# INLINE lift #-}
