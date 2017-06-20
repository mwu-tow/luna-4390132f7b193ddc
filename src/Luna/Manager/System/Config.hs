{-# LANGUAGE CPP #-}

module Luna.Manager.System.Config where

import Prologue hiding (FilePath)

import Luna.Manager.Config.Class
import Luna.Manager.System.Path
import Control.Monad.State.Layered
import qualified System.Directory as System


--------------------------
-- === SystemConfig === --
--------------------------

-- === Definition === --

data SystemConfig = SystemConfig { _localTempPath :: FilePath
                                 }
makeLenses ''SystemConfig


-- === Utils === --

getTmpPath, getDownloadPath :: MonadGetter SystemConfig m => m FilePath
getTmpPath      = view localTempPath <$> get @SystemConfig
getDownloadPath = getTmpPath

setTmpCwd :: (MonadGetter SystemConfig m, MonadIO m) => m ()
setTmpCwd = liftIO . System.setCurrentDirectory . convert =<< getTmpPath


-- === Instances === --
instance {-# OVERLAPPABLE #-} MonadIO m => MonadDefaultConfig SystemConfig sys m where
    defaultConfig = SystemConfig <$> (convert <$> liftIO System.getTemporaryDirectory)
