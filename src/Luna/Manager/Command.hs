module Luna.Manager.Command where

import Prologue

import Control.Monad.Raise
import Control.Monad.State.Layered
import Luna.Manager.Command.Options
import Luna.Manager.Component.Repository
import Luna.Manager.System.Host
import Luna.Manager.System.Env
import qualified Luna.Manager.Command.Install as Install
import           Luna.Manager.Command.Install (InstallConfig)
import qualified Luna.Manager.Command.CreatePackage as CreatePackage
import           Luna.Manager.Command.CreatePackage (PackageConfig)
import Shelly.Lifted (MonadSh)

chooseCommand :: (MonadIO m, MonadException SomeException m, MonadState Options m) => m ()
chooseCommand = do
    opts <- get @Options
    case opts ^. command of
        Install opt     -> evalDefHostConfigs @'[InstallConfig, EnvConfig, RepoConfig] $ Install.runInstaller opt
        MakePackage opt -> evalDefHostConfigs @'[PackageConfig, EnvConfig] $ CreatePackage.runCreatingPackage opt
        -- TODO: other commands
