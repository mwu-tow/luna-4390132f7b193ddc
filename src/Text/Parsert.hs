{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE Strict                 #-}
{-# EXT      InlineAll              #-}

module Text.Parsert where

import qualified Prelude as P
import Prologue hiding (catch, drop, (.), EitherT, mapEitherT, runEitherT, Either, Right, Left, isLeft, mapLeft)
import Control.Monad.Branch
import Control.Monad.Trans.Either.Strict
import Data.Either.Strict
import qualified Data.Text as Text
import Control.Monad.Trans.Maybe



class Monad m => MonadSuccessParser m where
    failed :: m a

instance {-# OVERLAPPABLE #-} (MonadSuccessParser m, MonadTrans t, Monad (t m))
      => MonadSuccessParser (t m) where
    failed = lift failed ; {-# INLINE failed #-}




(|||) :: XBool -> XBool -> XBool
(|||) !a !b = case a of
    XTrue  -> a
    XFalse -> b
{-# INLINE (|||) #-}

data XBool = XTrue | XFalse deriving (Show, Generic)
instance NFData XBool

data T a b = T !a !b deriving (Generic, Show, Functor)
instance (NFData a, NFData b) => NFData (T a b)

-- === Definition === --

newtype FailParser2 m a = FailParser2 { fromFailParser2 :: EitherT () m (T XBool a) } deriving (Functor)

instance Monad m => Applicative (FailParser2 m) where
    pure  = undefined
    (<*>) = undefined

instance Monad m => Monad (FailParser2 m) where
    return a = FailParser2 $ pure $ (T XFalse a) ; {-# INLINE return #-}
    FailParser2 ma >>= f = FailParser2 $ do
        T !b  a  <- ma
        T !b' a' <- fromFailParser2 $ f a
        return $! T (b' ||| b) a'
        -- return $! T XFalse a'
    {-# INLINE (>>=) #-}
    FailParser2 ma >> FailParser2 mb = FailParser2 $ do
        T !b  _  <- ma
        T !b' a' <- mb
        return $! T XFalse a'
        -- return $! T XTrue a'
    {-# INLINE (>>) #-}

instance MonadTrans (FailParser2) where
    lift m = FailParser2 $! lift $ fmap (T XFalse) m ; {-# INLINE lift #-}

-- instance MonadIO m => MonadIO (FailParser2 m) where
--     liftIO m = FailParser2 $! liftIO $ fmap (T XFalse) m ; {-# INLINE liftIO #-}

instance Monad m => MonadProgressParser3 (FailParser2 m) where
    returnProgress a = FailParser2 $! pure (T XFalse a) ; {-# INLINE returnProgress #-}


-- === Running === --

failParser2 :: m (Either () (T XBool a)) -> FailParser2 m a
failParser2 a = FailParser2 $ EitherT a ; {-# INLINE failParser2 #-}

runFailParser2 :: forall m a. FailParser2 m a -> m (Either () (T XBool a))
runFailParser2 f = runEitherT $ fromFailParser2 f ; {-# INLINE runFailParser2 #-}



instance Monad m => MonadSuccessParser (FailParser2 m) where failed = failParser2 $ pure $ Left () ; {-# INLINE failed #-}


class Monad m => MonadProgressParser3 m where
    returnProgress   :: a -> m a

instance {-# OVERLAPPABLE #-} (MonadProgressParser3 m, MonadTrans t, Monad (t m)) => MonadProgressParser3 (t m) where
    returnProgress a = lift $ returnProgress a ; {-# INLINE returnProgress #-}
