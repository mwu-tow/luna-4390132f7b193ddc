module Luna.Manager.Config.Class where

import Prologue
import Luna.Manager.System.Host
import Control.Monad.State.Layered


----------------------------
-- === Configurations === --
----------------------------

-- === Definition === --

type MonadDefaultConfig' cfg = MonadDefaultConfig cfg CurrentHost
class Monad m => MonadDefaultConfig cfg (system :: System) m where
    defaultConfig :: m cfg


-- === Utils === --

defaultConfigFor :: forall system cfg m. MonadDefaultConfig cfg system m => m cfg
defaultConfigFor = defaultConfig @cfg @system

currentSysDefaultConfig :: MonadDefaultConfig' cfg m => m cfg
currentSysDefaultConfig = defaultConfigFor @CurrentHost

evalDefConfigState :: forall s m a. MonadDefaultConfig' s m => StateT s m a -> m a
evalDefConfigState p = evalStateT @s p =<< currentSysDefaultConfig
