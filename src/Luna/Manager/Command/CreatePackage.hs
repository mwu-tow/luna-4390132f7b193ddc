{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.CreatePackage where

import Prologue hiding (FilePath)

import           Luna.Manager.System.Host
import           Luna.Manager.System.Env
import           Luna.Manager.System.Path
import           Luna.Manager.Archive
import           Luna.Manager.Component.Repository as Repo
import           Luna.Manager.Network
import           Luna.Manager.Command.Options (MakePackageOpts)
import qualified Luna.Manager.Command.Options as Opts

import Control.Lens.Aeson
import Control.Monad.Raise
import Control.Monad.State.Layered
import qualified Data.Map as Map
import qualified Data.Yaml as Yaml
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Shelly.Lifted as Shelly




data PackageConfig = PackageConfig { _defaultPackagePath :: FilePath
                                   , _studioName :: Text
                                   , _backendBuildPath :: FilePath
                                   , _frontendBuildPath :: FilePath
                                   , _runBuildPath :: FilePath
                                   , _atomPrepareScriptPath :: FilePath
                                   , _studioComponentsToCopy :: [FilePath]
                                   , _studioFolderName :: Text
                                   }

makeLenses ''PackageConfig


instance Monad m => MonadHostConfig PackageConfig 'Linux arch m where
    defaultHostConfig = return $ PackageConfig
        { _defaultPackagePath = "~/luna-package"
        , _studioName = "luna-studio"
        , _backendBuildPath = "./build/backend"
        , _frontendBuildPath = "./luna-studio"
        , _runBuildPath = "./packages"
        , _atomPrepareScriptPath = "./luna-studio/script/atom_prepare.py"
        , _studioComponentsToCopy = ["dist/bin", "env", "supervisor", "luna-studio/atom"]
        , _studioFolderName = "luna"
        }

instance Monad m => MonadHostConfig PackageConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg

instance Monad m => MonadHostConfig PackageConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & studioComponentsToCopy .~ ["dist/bin", "env", "supervisor", "luna-studio/atom"]

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

prepareAtomPkg :: MonadCreatePackage m => FilePath -> m ()
prepareAtomPkg path = do
    pkgConfig <- get @PackageConfig
    Shelly.shelly $ do
        Shelly.cmd $  path </> (pkgConfig ^. atomPrepareScriptPath)

runStackBuild :: MonadCreatePackage m => Text -> FilePath -> m ()
runStackBuild appName repoPath = do
    pkgConfig <- get @PackageConfig
    -- let repoPath = fromText $ pkgdesc ^. path
    case appName of
        "luna-studio" -> do
            let backendAbsolutePath  = repoPath </> (pkgConfig ^. backendBuildPath)
                frontendAbsolutePath = repoPath </> (pkgConfig ^. frontendBuildPath)
            Shelly.shelly $ buildCopyBins backendAbsolutePath
            Shelly.shelly $ build frontendAbsolutePath
            prepareAtomPkg repoPath

copyFromRepository :: MonadCreatePackage m => Text -> FilePath -> m ()
copyFromRepository appName repoPath = do
    pkgConfig <- get @PackageConfig
    packageRepoFolder <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText (pkgConfig ^. studioFolderName)
    Shelly.shelly $ Shelly.mkdir_p packageRepoFolder
    let expandedCopmponents = (repoPath </> ) <$> (pkgConfig ^. studioComponentsToCopy)
    Shelly.shelly $ (flip Shelly.cp_r packageRepoFolder) `mapM_` expandedCopmponents

downloadAndUnpackDependency :: MonadCreatePackage m => ResolvedPackage -> m ()
downloadAndUnpackDependency resolvedPackage = do
    pkgConfig <- get @PackageConfig
    let depName = resolvedPackage ^. header . name
    pkgFolderPath <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText depName
    print $ show pkgFolderPath
    downloadedPkg <- downloadFromURL $ resolvedPackage ^. desc . path
    print $ show downloadedPkg
    unpacked <- unpackArchive downloadedPkg
    print $ show unpacked
    Shelly.shelly $ Shelly.mkdir_p $ pkgFolderPath
    Shelly.shelly $ copyDir unpacked pkgFolderPath

data ResolvedPackageMap  = ResolvedPackageMap { _appName    :: Text
                                              , _appDesc    :: PackageDesc
                                              , _pkgsToPack :: [ResolvedPackage]
                                              } deriving (Show)
makeLenses ''ResolvedPackageMap

createPkg :: MonadCreatePackage m => ResolvedPackageMap -> m ()
createPkg resolvedApp = do
    runStackBuild (resolvedApp ^. appName) $ fromText (resolvedApp ^. appDesc . path)
    copyFromRepository (resolvedApp ^. appName) $ fromText (resolvedApp ^. appDesc . path)
    mapM_ downloadAndUnpackDependency (resolvedApp ^. pkgsToPack)



runCreatingPackage :: MonadCreatePackage m => MakePackageOpts -> m ()
runCreatingPackage opts = do
    repo <- parseConfig (opts ^. Opts.cfgPath)
    let appsToPack = repo ^. apps
        appPkg  = map (\app -> (app, Map.lookup app (repo ^. packages))) appsToPack
        pkgList = map (\(name, package) -> (name, fromMaybe (error $ convert $ "package undefined in yaml file: " <> name) package)) appPkg
        pkgMap  = map (\(name, package) -> (name, fromMaybe (error $ "no package description") $ Map.lookup currentSysDesc $ snd $ head $ toList $ package ^. versions) ) pkgList
        resolvedMap = map (\(name,pkgDesc) -> ResolvedPackageMap name pkgDesc $ snd $ resolve repo pkgDesc) pkgMap -- TODO zrób to bezpiecznie!

    mapM_ createPkg resolvedMap
    -- mapM_ downloadAndUnpackDependency $ head resolvedMap
    print $ show resolvedMap


    return ()
