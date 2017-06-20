module Luna.Manager.Config.Class where

import Prologue
import Luna.Manager.System.Host
import Control.Monad.State.Layered


----------------------------
-- === Configurations === --
----------------------------

-- === Definition === --

class Monad m => MonadSystemConfig cfg (system :: System) (arch :: SysArch) m where
    defaultConfig :: m cfg


-- === Utils === --

defaultConfigFor :: forall system arch cfg m. MonadSystemConfig cfg system arch m => m cfg
defaultConfigFor = defaultConfig @cfg @system @arch

type MonadSystemConfig' cfg = MonadSystemConfig cfg CurrentHost CurrentArch
defSystemConfig :: MonadSystemConfig' cfg m => m cfg
defSystemConfig = defaultConfigFor @CurrentHost @CurrentArch

evalDefSystemConfig :: forall s m a. MonadSystemConfig' s m => StateT s m a -> m a
evalDefSystemConfig p = evalStateT @s p =<< defSystemConfig
