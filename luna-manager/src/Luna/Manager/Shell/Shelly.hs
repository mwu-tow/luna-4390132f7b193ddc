module Luna.Manager.Shell.Shelly (module Luna.Manager.Shell.Shelly, module X) where

import Prologue hiding (FilePath)

import Luna.Manager.System.Host
import Control.Monad.State.Layered
import Shelly.Lifted           as X hiding (mv)
import qualified Shelly.Lifted as S

deriving instance MonadSh m => MonadSh (StateT s m)

mv :: (MonadIO m, MonadSh m) => FilePath -> FilePath -> m ()
mv src dst = case currentHost of
    Linux   -> cmd "mv" src dst
    Darwin  -> cmd "mv" src dst
    Windows -> S.mv src dst
