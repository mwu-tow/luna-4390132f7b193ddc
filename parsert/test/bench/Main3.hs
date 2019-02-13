{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}

module Main where

import Prelude hiding (Either, Left, Right)
import qualified Prelude as P
import Control.Monad.Trans
import Control.DeepSeq (NFData, force)

import Data.Monoid
import Criterion.Main
import Control.Monad.State.Strict
import qualified Data.Text as Text
import           Data.Text (Text)
import Control.Monad.Identity
import GHC.Generics
import GHC.IO          as X (evaluate)
import Data.String (fromString)


-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- | WARNING: -XStrict enabled in this file !!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

------------------------
-- === Primitives === --
------------------------

-- === Either === --

data    Either  e   a = Left e | Right a deriving (Eq, Generic, Ord, Read, Show, Functor)
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Monad m => Functor (EitherT e m) where
  fmap f = undefined

instance Monad m => Applicative (EitherT e m) where
  pure  = undefined ; {-# INLINE pure #-}
  (<*>) = undefined ; {-# INLINE (<*>) #-}

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


-- === Bool === --

data XBool = XTrue | XFalse deriving (Show, Generic)

-- (|||) :: XBool -> XBool -> XBool
-- (|||) !a !b = case a of
--     XTrue  -> a
--     XFalse -> b
-- {-# INLINE (|||) #-}

(|||) :: XBool -> XBool -> XBool
(|||) !a !b = case (a,b) of
    (XTrue, XTrue) -> XTrue
    (XTrue, XFalse) -> XFalse
    (XFalse, XTrue) -> XFalse
    (XFalse, XFalse) -> XTrue
    -- XTrue  -> a
    -- XFalse -> b
{-# INLINE (|||) #-}


-- === Tuple === --

data T a b = T !a !b deriving (Generic, Show, Functor)


------------------------
-- === FailParser === --
------------------------

-- === Definition === --

newtype FailParser m a = FailParser { fromFailParser :: EitherT () m (T XBool a) } deriving (Functor)

instance Monad m => Applicative (FailParser m) where
    pure  = undefined
    (<*>) = undefined

instance Monad m => Monad (FailParser m) where
    return a = FailParser $ return $ (T XFalse a) ; {-# INLINE return #-}
    FailParser ma >>= f = FailParser $ do
        T !b  !a  <- ma
        T !b' !a' <- fromFailParser $ f a
        return $ T b' a'
    {-# INLINE (>>=) #-}
    _ >> _ = undefined ; {-# INLINE (>>) #-}


-- === Running === --

failParser :: m (Either () (T XBool a)) -> FailParser m a
failParser a = FailParser $ EitherT a ; {-# INLINE failParser #-}

runFailParser :: forall m a. FailParser m a -> m (Either () (T XBool a))
runFailParser f = runEitherT $ fromFailParser f ; {-# INLINE runFailParser #-}


-- === MonadFailedParser === --

class Monad m => MonadFailedParser m where
    failed :: m a

instance {-# OVERLAPPABLE #-} (MonadFailedParser m, MonadTrans t, Monad (t m))
      => MonadFailedParser (t m) where
    failed = lift failed ; {-# INLINE failed #-}

instance Monad m => MonadFailedParser (FailParser m) where
    failed = failParser $ return $ Left () ; {-# INLINE failed #-}


-----------------------
-- === Main loop === --
-----------------------

parserLoop :: StateT Text (FailParser Identity) Bool
parserLoop = parserStep >> parserLoop

parserStep :: StateT Text (FailParser Identity) Char
parserStep = get >>= \s -> case Text.uncons s of
    Just (!t, !s') -> if t == 'a' then put s' >> return t else failed
    Nothing        -> failed
{-# INLINE parserStep #-}


-- === Criterion === --

instance NFData XBool
instance (NFData l, NFData r) => NFData (Either l r)
instance (NFData a, NFData b) => NFData (T a b)

genText :: Int -> Text
genText i = fromString $ replicate i 'a' ; {-# INLINE genText #-}

a_parsing_main :: IO ()
a_parsing_main = do
    defaultMain
        [ env (return $ genText $ 10^6) $ bench "a*" . nf (runIdentity . runFailParser . evalStateT parserLoop)
        ]


main = a_parsing_main
