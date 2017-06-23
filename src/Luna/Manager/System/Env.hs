{-# LANGUAGE CPP #-}

module Luna.Manager.System.Env where

import Prologue hiding (FilePath)

import Luna.Manager.System.Host
import Luna.Manager.System.Path
import Control.Monad.State.Layered
import qualified System.Directory as System


--------------------------
-- === EnvConfig === --
--------------------------

-- === Definition === --

data EnvConfig = EnvConfig { _localTempPath :: FilePath
                           }
makeLenses ''EnvConfig


-- === Utils === --

getHomePath :: MonadIO m => m FilePath
getHomePath = do
    home <- liftIO $ System.getHomeDirectory
    return $ convert home

getTmpPath, getDownloadPath :: MonadGetter EnvConfig m => m FilePath
getTmpPath      = view localTempPath <$> get @EnvConfig
getDownloadPath = getTmpPath

setTmpCwd :: (MonadGetter EnvConfig m, MonadIO m) => m ()
setTmpCwd = liftIO . System.setCurrentDirectory . convert =<< getTmpPath

createDir :: MonadIO m => FilePath -> m ()
createDir path = liftIO $ System.createDirectory $ convert path

createSymLink :: MonadIO m => FilePath -> FilePath -> m ()
createSymLink src dst = liftIO $ System.createFileLink (convert src) (convert dst)

createDirIfMissingTrue :: MonadIO m => FilePath-> m ()
createDirIfMissingTrue path = liftIO $ System.createDirectoryIfMissing True $ convert path

-- === Instances === --
instance {-# OVERLAPPABLE #-} MonadIO m => MonadHostConfig EnvConfig sys arch m where
    defaultHostConfig = EnvConfig <$> (convert <$> liftIO System.getTemporaryDirectory)
