{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Data.Either.Strict where

import GHC.Base
import GHC.Show
import GHC.Read

import Data.Type.Equality
import GHC.Generics

data  Either a b  =  Left a | Right b
  deriving (Eq, Generic, Ord, Read, Show)

instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)
    {-# INLINE fmap #-}

instance Applicative (Either e) where
    pure          = Right ; {-# INLINE pure #-}
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r
    {-# INLINE (<*>) #-}

instance Monad (Either e) where
    return = pure ; {-# INLINE return #-}
    Left  l >>= _ = Left l
    Right r >>= k = k r
    {-# INLINE (>>=) #-}


either                  :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
{-# INLINE either #-}


mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)
{-# INLINE mapBoth #-}

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = mapBoth f id
{-# INLINE mapLeft #-}

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False
{-# INLINE isLeft #-}
