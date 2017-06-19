module Luna.Manager.Installer where

import Prologue

import Luna.Manager.Config.Class
import Luna.Manager.System.Host


---------------------------------
-- === Installation config === --
---------------------------------

-- === Definition === --

data InstallConfig = InstallConfig { _execName        :: Text
                                   , _defaultConfPath :: FilePath
                                   , _defaultBinPath  :: FilePath
                                   , _localName       :: Text
                                   }
makeLenses ''InstallConfig

-- === Instances === --

instance {-# OVERLAPPABLE #-} Monad m => MonadDefaultConfig InstallConfig sys m where
    defaultConfig = return $ InstallConfig
        { _execName        = "luna-studio"
        , _defaultConfPath = "~/.luna"
        , _defaultBinPath  = "~/.luna-bin"
        , _localName       = "local"
        }

instance Monad m => MonadDefaultConfig InstallConfig 'MacOS m where
    defaultConfig = reconfig <$> defaultConfigFor @Linux where
        reconfig cfg = cfg & execName       .~ "LunaStudio"
                           & defaultBinPath .~ "~/Applications"


instance Monad m => MonadDefaultConfig InstallConfig 'Windows m where
    defaultConfig = reconfig <$> defaultConfigFor @Linux where
        reconfig cfg = cfg & execName       .~ "LunaStudio"
                           & defaultBinPath .~ "C:\\ProgramFiles"
