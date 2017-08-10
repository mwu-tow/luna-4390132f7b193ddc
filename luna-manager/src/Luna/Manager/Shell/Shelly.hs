module Luna.Manager.Shell.Shelly (module Luna.Manager.Shell.Shelly, module X) where

import Prologue
import Control.Monad.State.Layered
import Shelly.Lifted as X

deriving instance MonadSh m => MonadSh (StateT s m)
