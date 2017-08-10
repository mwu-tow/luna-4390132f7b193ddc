{-# LANGUAGE TypeInType #-}

module Control.Monad.Raise (module Control.Monad.Raise, module X) where

-- FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
-- FIXME [WD]: refactor the whole file out before the release

import Prelude
import Data.Kind

import qualified Luna.Manager.Shell.Shelly as Shelly

import Control.Lens.Utils
import Control.Exception   as X (Exception, SomeException, toException)
import Control.Monad.Catch (MonadThrow, throwM)

import Control.Monad              (join)
import Data.Constraint            (Constraint)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans        (MonadTrans, lift)
import qualified Control.Exception.Safe as Exception

-------------------------------
-- === Exception raising === --
-------------------------------

-- === MonadException === --

type ExceptT' = ExceptT SomeException

class (Monad m, Exception e) => MonadException e m where
    raise :: forall a. e -> m a

type family MonadExceptions es m :: Constraint where
    MonadExceptions '[]       m = ()
    MonadExceptions (e ': es) m = (MonadException e m, MonadExceptions es m)


-- === Utils === --

handle :: Monad m => (e -> m a) -> ExceptT e m a -> m a
handle f = join . fmap (either f return) . runExceptT

handleAll :: Monad m => (SomeException -> m a) -> ExceptT' m a -> m a
handleAll = handle

rethrow :: (MonadThrow m, Exception e) => ExceptT e m a -> m a
rethrow = handle throwM

rethrowAll :: MonadThrow m => ExceptT' m a -> m a
rethrowAll = rethrow

tryAll :: ExceptT' m a -> m (Either SomeException a)
tryAll = runExceptT


-- === Throws === --

type family   Throws (e :: k) (m :: * -> *) :: Constraint
type instance Throws e m = MonadExceptions e m
type instance Throws e m = MonadException  e m


-- === Intsances === --

-- Default MonadException instances
instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, MonadException e m)
                                                     => MonadException e (t                     m) where raise = lift . raise
instance {-# OVERLAPPABLE #-} (Monad m, Exception e) => MonadException e (ExceptT e             m) where raise = throwE
instance {-# OVERLAPPABLE #-} (Monad m, Exception e) => MonadException e (ExceptT SomeException m) where raise = throwE . toException
instance                      (Monad m)              => MonadException SomeException (ExceptT SomeException m) where raise = throwE
instance                      Exception e            => MonadException e IO where raise = Exception.throwM
instance                      Exception e            => MonadException e Shelly.Sh where raise = Exception.throwM


-- === Utils === --

tryJust :: MonadException e m => e -> Maybe a -> m a
tryJust e = maybe (raise e) return

tryRight :: MonadException e m => (l -> e) -> Either l r -> m r
tryRight f = \case
    Right r -> return r
    Left  l -> raise $ f l

tryRight' :: forall l m r. (MonadException SomeException m, Exception l) => Either l r -> m r
tryRight' = tryRight toException

raise' :: (MonadException SomeException m, Exception e) => forall a. e -> m a
raise' = raise . toException
