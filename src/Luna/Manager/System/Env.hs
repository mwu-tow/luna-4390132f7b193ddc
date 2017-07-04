{-# LANGUAGE CPP #-}

module Luna.Manager.System.Env where

import Prologue hiding (FilePath, fromText, toText)

import Luna.Manager.System.Host
import Filesystem.Path.CurrentOS
import Control.Monad.State.Layered
import qualified System.Directory as System
import Control.Monad.Raise
import qualified Shelly.Lifted as Shelly

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

getTmpPath, getDownloadPath :: (MonadIO m, MonadGetter EnvConfig m) => m FilePath
getTmpPath      = do
    tmp <- view localTempPath <$> get @EnvConfig
    Shelly.shelly $ Shelly.mkdir_p tmp
    return tmp
getDownloadPath = getTmpPath


setTmpCwd :: (MonadGetter EnvConfig m, MonadIO m) => m ()
setTmpCwd = liftIO . System.setCurrentDirectory . encodeString =<< getTmpPath

createSymLink :: MonadIO m => FilePath -> FilePath -> m ()
createSymLink src dst = liftIO $ System.createFileLink (encodeString src) (encodeString dst)



copyDir :: Shelly.MonadSh m => FilePath -> FilePath -> m ()-- copy the content of the source directory
copyDir src dst = do
    listedDirectory <- Shelly.ls src
    mapM_ (flip Shelly.cp_r dst) listedDirectory

-- === Instances === --
instance {-# OVERLAPPABLE #-} MonadIO m => MonadHostConfig EnvConfig sys arch m where
    defaultHostConfig = EnvConfig <$> tmp where
        tmp =  (</> "tmp/luna") <$> decodeString <$> liftIO System.getTemporaryDirectory
