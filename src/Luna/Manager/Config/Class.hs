module Luna.Manager.Config.Class where

import Prologue
import Luna.Manager.System.Host


----------------------------
-- === Configurations === --
----------------------------

-- === Definition === --

class Monad m => MonadDefaultConfig cfg (system :: System) m where
    defaultConfig :: m cfg


-- === Utils === --

defaultConfigFor :: forall system cfg m. MonadDefaultConfig cfg system m => m cfg
defaultConfigFor = defaultConfig @cfg @system

currentSysDefaultConfig :: MonadDefaultConfig cfg CurrentHost m => m cfg
currentSysDefaultConfig = defaultConfigFor @CurrentHost
