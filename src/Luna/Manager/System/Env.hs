{-# LANGUAGE CPP #-}

module Luna.Manager.System.Env where

import Prologue hiding (FilePath, fromText, toText)

import Luna.Manager.System.Host
import Filesystem.Path.CurrentOS
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
    return $ fromText $ convert home

getCurrentPath :: MonadIO m => m FilePath
getCurrentPath = do
    current <- liftIO $ System.getCurrentDirectory
    return $ fromText $ convert current

getTmpPath, getDownloadPath :: MonadGetter EnvConfig m => m FilePath
getTmpPath      = view localTempPath <$> get @EnvConfig
getDownloadPath = getTmpPath


setTmpCwd :: (MonadGetter EnvConfig m, MonadIO m) => m ()
setTmpCwd = liftIO . System.setCurrentDirectory . encodeString =<< getTmpPath

createDir :: MonadIO m => FilePath -> m ()
createDir path = liftIO $ System.createDirectory $ encodeString path

createSymLink :: MonadIO m => FilePath -> FilePath -> m ()
createSymLink src dst = liftIO $ System.createFileLink (encodeString src) (encodeString dst)

createDirIfMissingTrue :: MonadIO m => FilePath-> m ()
createDirIfMissingTrue path = liftIO $ System.createDirectoryIfMissing True $ encodeString path

-- === Instances === --
instance {-# OVERLAPPABLE #-} MonadIO m => MonadHostConfig EnvConfig sys arch m where
    defaultHostConfig = EnvConfig <$> (decodeString <$> liftIO System.getTemporaryDirectory)
