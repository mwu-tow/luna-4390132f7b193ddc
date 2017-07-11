{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.CreatePackage where

import Prologue hiding (FilePath)

import           Luna.Manager.System.Host
import           Luna.Manager.System.Env
import           Luna.Manager.Component.Repository as Repo
import           Luna.Manager.Network
import           Luna.Manager.Command.Options (MakePackageOpts)
import qualified Luna.Manager.Command.Options as Opts

import Control.Lens.Aeson
import Control.Monad.Raise
import Control.Monad.State.Layered
import qualified Data.Yaml as Yaml
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Shelly.Lifted as Shelly




data PackageConfig = PackageConfig { _defaultPackagePath :: FilePath
                                   , _studioName :: Text
                                   , _backendBuildPath :: FilePath
                                   , _frontendBuildPath :: FilePath
                                   , _runBuildPath :: FilePath
                                   , _atomPrepareScriptPath :: FilePath
                                   }

makeLenses ''PackageConfig


instance Monad m => MonadHostConfig PackageConfig 'Linux arch m where
    defaultHostConfig = return $ PackageConfig
        { _defaultPackagePath = "~/luna-package"
        , _studioName = "luna-studio"
        , _backendBuildPath = "./build/backend"
        , _frontendBuildPath = "./luna-studio"
        , _runBuildPath = "./packages"
        , _atomPrepareScriptPath = "./luna-studio/script/atom_prep.py"
        }

instance Monad m => MonadHostConfig PackageConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg

instance Monad m => MonadHostConfig PackageConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg

type MonadCreatePackage m = (MonadStates '[EnvConfig, PackageConfig] m, MonadNetwork m)

parseConfig :: (MonadIO m, MonadException SomeException m) => Text -> m Repo
parseConfig cfgPath =  tryRight' =<< liftIO (Yaml.decodeFileEither $ convert cfgPath)

build :: Shelly.MonadSh m => FilePath -> m ()
build path = do
    Shelly.cd path
    Shelly.cmd "stack" "build"

buildCopyBins :: Shelly.MonadSh m => FilePath -> m ()
buildCopyBins path = do
    Shelly.cd path
    Shelly.cmd "stack" "build" "--copy-bins" "--fast"

runStackBuild :: MonadCreatePackage m => Text -> FilePath -> m ()
runStackBuild appName repoPath = do
    pkgConfig <- get @PackageConfig
    case appName of
        "luna-studio" -> do
            let backendAbsolutePath = repoPath </> (pkgConfig ^. backendBuildPath)
                frontendAbsolutePath = repoPath </> (pkgConfig ^. frontendBuildPath)
            Shelly.shelly $ buildCopyBins backendAbsolutePath
            Shelly.shelly $ build frontendAbsolutePath




runCreatingPackage :: MonadCreatePackage m => MakePackageOpts -> m ()
runCreatingPackage opts = do
    repo <- parseConfig (opts ^. Opts.cfgPath)
    print $ show repo

    return ()
