{-# LANGUAGE ScopedTypeVariables #-}
module Luna.Manager.Shell.Shelly (module Luna.Manager.Shell.Shelly, module X) where

import Prologue hiding (FilePath)

import Luna.Manager.System.Host
import Control.Monad.State.Layered
import qualified Control.Monad.State.Lazy as S
import Shelly.Lifted           as X hiding (mv)
import qualified Shelly.Lifted as S

import Unsafe.Coerce

deriving instance MonadSh        m => MonadSh        (StateT s m)

-- Maybe we could simplify it in GHC 8.2 ?
instance MonadShControl m => MonadShControl (StateT s m) where
    newtype ShM (StateT s m) a = StateTShM { fromShM :: ShM (S.StateT s m) a }
    restoreSh shm = StateT $ restoreSh (fromShM shm)
    liftShWith (f :: ((forall x. StateT s m x -> Sh (ShM (StateT s m) x)) -> Sh a)) = StateT $ liftShWith f' where
        f' :: (forall x. S.StateT s m x -> Sh (ShM (S.StateT s m) x)) -> Sh a
        f' h = f h' where
            h' :: (forall x. StateT s m x -> Sh (ShM (StateT s m) x))
            h' s = fmap StateTShM $ h (unwrap s)


mv :: (MonadIO m, MonadSh m) => FilePath -> FilePath -> m ()
mv src dst = case currentHost of
    Linux   -> cmd "mv" src dst
    Darwin  -> cmd "mv" src dst
    Windows -> S.mv src dst
