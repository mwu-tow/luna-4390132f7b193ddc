{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.CreatePackage where

import Prologue hiding (FilePath)

import           Luna.Manager.System.Host
import           Luna.Manager.System.Env
import           Luna.Manager.System.Path
import Luna.Manager.System (makeExecutable)
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
import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString)
import qualified Shelly.Lifted as Shelly
import qualified System.Process.Typed as Process



data PackageConfig = PackageConfig { _defaultPackagePath :: FilePath
                                   , _studioName :: Text
                                   , _backendBuildPath :: FilePath
                                   , _frontendBuildPath :: FilePath
                                   , _runBuildPath :: FilePath
                                   , _atomPrepareScriptPath :: FilePath
                                   , _studioComponentsToCopy :: [FilePath]
                                   , _studioFolderName :: Text
                                   , _studioUtilsFolder :: Text
                                   , _lunaUtilsFolder   :: Text
                                   , _logoFileName :: Text
                                   , _desktopFileName :: Text
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
        , _studioComponentsToCopy = ["dist/bin", "env", "supervisor", "luna-studio/atom", "resources"]
        , _studioFolderName = "luna"
        , _studioUtilsFolder = "resources"
        , _lunaUtilsFolder = "docs"
        , _logoFileName = "logo.png"
        , _desktopFileName = "app.desktop"
        }

instance Monad m => MonadHostConfig PackageConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg

instance Monad m => MonadHostConfig PackageConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & studioComponentsToCopy .~ ["dist/bin", "env", "supervisor", "luna-studio/atom"]

data ResolvedPackageMap  = ResolvedPackageMap { _appName    :: Text
                                              , _appDesc    :: PackageDesc
                                              , _pkgsToPack :: [ResolvedPackage]
                                              } deriving (Show)
makeLenses ''ResolvedPackageMap

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
    let studioAppName = pkgConfig ^. studioName
    case appName of
        studioAppName -> do
            let backendAbsolutePath  = repoPath </> (pkgConfig ^. backendBuildPath)
                frontendAbsolutePath = repoPath </> (pkgConfig ^. frontendBuildPath)
            Shelly.shelly $ buildCopyBins backendAbsolutePath
            Shelly.shelly $ build frontendAbsolutePath
            prepareAtomPkg repoPath

copyFromRepository :: MonadCreatePackage m => Text -> FilePath -> m ()
copyFromRepository appName repoPath = do
    pkgConfig <- get @PackageConfig
    packageRepoFolder <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText appName </> fromText (pkgConfig ^. studioFolderName)
    Shelly.shelly $ Shelly.mkdir_p packageRepoFolder
    let expandedCopmponents = (repoPath </> ) <$> (pkgConfig ^. studioComponentsToCopy)
    Shelly.shelly $ (flip Shelly.cp_r packageRepoFolder) `mapM_` expandedCopmponents

downloadAndUnpackDependency :: MonadCreatePackage m => Text -> ResolvedPackage -> m ()
downloadAndUnpackDependency appName resolvedPackage = do
    pkgConfig <- get @PackageConfig
    let depName = resolvedPackage ^. header . name
    pkgFolderPath <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText appName </> fromText depName
    print $ show pkgFolderPath
    downloadedPkg <- downloadFromURL $ resolvedPackage ^. desc . path
    print $ show downloadedPkg
    unpacked <- unpackArchive downloadedPkg
    print $ show unpacked
    Shelly.shelly $ Shelly.cmd "mv" unpacked pkgFolderPath

runApm :: (MonadIO m, Shelly.MonadSh m) => FilePath -> FilePath -> FilePath -> m ()
runApm apmPath atomHomePath onigurumaPath = do
    print $ show apmPath
    print $ show atomHomePath
    Shelly.cd atomHomePath
    let nodeModulesAtomHome = atomHomePath </> "node_modules"
        onigurumaAtomHome = atomHomePath </> "node_modules" </> "oniguruma"
    Shelly.mkdir_p nodeModulesAtomHome
    -- Shelly.rm_rf onigurumaAtomHome
    Shelly.cp_r onigurumaPath nodeModulesAtomHome
    Shelly.setenv "ATOM_HOME" $ Shelly.toTextIgnore atomHomePath
    Shelly.cmd apmPath "install" "."

createAppimage :: MonadCreatePackage m => Text -> m ()
createAppimage appName = do
    pkgConfig <- get @PackageConfig
    tmpAppDirPath <- expand $ (pkgConfig ^. defaultPackagePath) </> "appimage" </> fromText appName </> fromText (appName <> ".AppDir")
    tmpAppPath <- expand $ (pkgConfig ^. defaultPackagePath) </> "appimage" </> fromText appName

    srcPkgPath <- (expand $ (pkgConfig ^. defaultPackagePath) </> fromText appName)
    srcLibPath <- (expand $ (pkgConfig ^. defaultPackagePath) </> fromText appName </> "zmq" </> "lib64")
    let dstPkgPath = (tmpAppDirPath </> "usr" </> "bin")
        dstLibPath = (tmpAppDirPath </> "usr" </> "lib")

    Shelly.shelly $ Shelly.mkdir_p tmpAppDirPath
    Shelly.shelly $ Shelly.cd tmpAppPath
    functions <- downloadWithProgressBar "https://github.com/probonopd/AppImages/raw/master/functions.sh" tmpAppPath
    Shelly.shelly $ do
        Shelly.mkdir_p $ tmpAppDirPath </> "usr" </> "bin"
        Shelly.mkdir_p $ tmpAppDirPath </> "usr" </> "lib"

    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ ". " ++ (encodeString functions) ++ " && " ++ "get_apprun"

    logoFile <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText appName </> fromText (pkgConfig ^. studioFolderName) </> fromText (pkgConfig ^. studioUtilsFolder) </> fromText (pkgConfig ^. logoFileName)
    desktopFile <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText appName </> fromText (pkgConfig ^. studioFolderName) </> fromText (pkgConfig ^. studioUtilsFolder) </> fromText (pkgConfig ^. desktopFileName)

    Shelly.shelly $ do
        Shelly.cp logoFile $ tmpAppDirPath </> fromText (appName <> ".png")
        Shelly.cp desktopFile $ tmpAppDirPath </> fromText (appName <> ".desktop")
        copyDir srcPkgPath dstPkgPath
        copyDir srcLibPath dstLibPath
    appWrapper <- downloadWithProgressBar "https://raw.githubusercontent.com/probonopd/AppImageKit/master/desktopintegration" tmpAppDirPath
    let dstWrapper = (dstPkgPath </> fromText (appName <> ".wrapper"))
    Shelly.shelly $ Shelly.mv appWrapper dstWrapper
    makeExecutable dstWrapper
    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ "sed -i -e \"s|Exec=" ++ (convert appName) ++ "|Exec=" ++ (convert appName) ++".wrapper|g\" " ++ (convert appName) ++ ".desktop"
    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppPath) $ Process.setEnv [("APP", (convert appName))] $ Process.shell $ ". " ++ (encodeString functions) ++ " && " ++ "generate_type2_appimage"



createPkg :: MonadCreatePackage m => ResolvedPackageMap -> m ()
createPkg resolvedApp = do
    pkgConfig <- get @PackageConfig
    runStackBuild (resolvedApp ^. appName) $ fromText (resolvedApp ^. appDesc . path)
    copyFromRepository (resolvedApp ^. appName) $ fromText (resolvedApp ^. appDesc . path)
    mapM_ (downloadAndUnpackDependency (resolvedApp ^. appName)) (resolvedApp ^. pkgsToPack)
    let studio = pkgConfig ^. studioName
    case (resolvedApp ^. appName) of
        studio -> do
            apmPath <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText (resolvedApp ^. appName) </> "atom" </> "usr" </> "share" </> "atom" </> "resources" </> "app" </> "apm" </> "bin" </> "apm"
            onigurumaPath <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText (resolvedApp ^. appName) </> "atom" </> "usr" </> "share" </> "atom" </> "resources" </> "app" </> "node_modules" </> "oniguruma"
            atomHomePath <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText (resolvedApp ^. appName) </> "luna-atom" </> "packages" </> fromText (resolvedApp ^. appName)
            atomDirInStudio <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText (resolvedApp ^. appName) </> fromText (pkgConfig ^. studioFolderName) </> "atom"
            Shelly.shelly $ Shelly.mkdir_p atomHomePath
            Shelly.shelly $ copyDir atomDirInStudio atomHomePath
            Shelly.shelly $ runApm apmPath atomHomePath onigurumaPath
        otherwise -> return ()
    mainAppDir <- expand $ (pkgConfig ^. defaultPackagePath) </> fromText (resolvedApp ^. appName) </> fromText (resolvedApp ^. appName)
    Shelly.shelly $ Shelly.cp "./executables/run"  mainAppDir
    createAppimage (resolvedApp ^. appName)



runCreatingPackage :: MonadCreatePackage m => MakePackageOpts -> m ()
runCreatingPackage opts = do
    repo <- parseConfig (opts ^. Opts.cfgPath)
    let appsToPack = repo ^. apps
        appPkg  = map (\app -> (app, Map.lookup app (repo ^. packages))) appsToPack
        pkgList = map (\(name, package) -> (name, fromMaybe (error $ convert $ "package undefined in yaml file: " <> name) package)) appPkg
        pkgMap  = map (\(name, package) -> (name, fromMaybe (error $ "no package description") $ Map.lookup currentSysDesc $ snd $ head $ toList $ package ^. versions) ) pkgList
        resolvedMap = map (\(name,pkgDesc) -> ResolvedPackageMap name pkgDesc $ snd $ resolve repo pkgDesc) pkgMap -- TODO zrób to bezpiecznie!

    mapM_ createPkg resolvedMap


    return ()
