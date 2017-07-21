{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.CreatePackage where



import           Control.Lens.Aeson
import           Control.Monad.Raise
import           Control.Monad.State.Layered
import           Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, decodeString, parent,splitDirectories,null, filename)
import           Luna.Manager.Archive
import           Luna.Manager.Command.Options (MakePackageOpts)
import           Luna.Manager.Component.Repository as Repo
import           Luna.Manager.Network
import           Luna.Manager.System (makeExecutable)
import           Luna.Manager.System.Env
import           Luna.Manager.System.Host
import           Luna.Manager.System.Path
import           Luna.Manager.Component.Version (Version)
import           Luna.Manager.Component.Pretty
import           Prologue hiding (FilePath)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import qualified Luna.Manager.Command.Options as Opts
import qualified Shelly.Lifted as Shelly
import qualified System.Process.Typed as Process


data PackageConfig = PackageConfig { _defaultPackagePath     :: FilePath
                                   , _buildScriptPath        :: FilePath
                                   , _thirdPartyPath         :: FilePath
                                   , _libPath                :: FilePath
                                   , _componentsToCopy       :: FilePath
                                   , _configFolder           :: FilePath
                                   , _binFolder              :: FilePath
                                   , _binsPrivate            :: FilePath
                                   , _binsPublic             :: FilePath
                                   , _utilsFolder            :: FilePath
                                   , _logoFileName           :: Text
                                   , _desktopFileName        :: Text
                                   , _versionFileName        :: FilePath
                                   }

makeLenses ''PackageConfig


instance Monad m => MonadHostConfig PackageConfig 'Linux arch m where
    defaultHostConfig = return $ PackageConfig
        { _defaultPackagePath = "dist-package"
        , _buildScriptPath    = "./scripts-build/build.py"
        , _thirdPartyPath     = "third-party"
        , _libPath            = "lib"
        , _componentsToCopy   = "dist"
        , _configFolder       = "config"
        , _binFolder          = "bin"
        , _binsPrivate        = "private"
        , _binsPublic         = "public"
        , _utilsFolder        = "resources"
        , _logoFileName       = "logo.png"
        , _desktopFileName    = "app.desktop"
        , versionFileName     = "version.txt"
        }

instance Monad m => MonadHostConfig PackageConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg

instance Monad m => MonadHostConfig PackageConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg



type MonadCreatePackage m = (MonadStates '[EnvConfig, PackageConfig] m, MonadNetwork m)



runBuild :: MonadCreatePackage m => FilePath -> m ()
runBuild repoPath = do
    pkgConfig <- get @PackageConfig
    buildPath <- expand $ repoPath </> (pkgConfig ^. buildScriptPath)
    Shelly.shelly $ Shelly.cmd buildPath


copyFromRepository :: MonadCreatePackage m => Text -> FilePath -> m ()
copyFromRepository appName repoPath = do
    pkgConfig <- get @PackageConfig
    packageRepoFolder <- expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
    let expandedCopmponents = repoPath </> (pkgConfig ^. componentsToCopy)
    Shelly.shelly $ Shelly.mkdir_p packageRepoFolder
    Shelly.shelly $ copyDir expandedCopmponents packageRepoFolder

downloadAndUnpackDependency :: MonadCreatePackage m => FilePath -> ResolvedPackage -> m ()
downloadAndUnpackDependency repoPath resolvedPackage = do
    pkgConfig <- get @PackageConfig
    let depName     = resolvedPackage ^. header . name
        packageType = resolvedPackage ^. resolvedAppType

    thirdPartyFullPath <- expand $ repoPath </> (pkgConfig ^. componentsToCopy) </> (pkgConfig ^. thirdPartyPath)
    libFullPath        <- expand $ repoPath </> (pkgConfig ^. componentsToCopy) </> (pkgConfig ^. libPath)
    downloadedPkg      <- downloadFromURL $ resolvedPackage ^. desc . path
    unpacked           <- unpackArchive downloadedPkg
    Shelly.shelly $ Shelly.mkdir_p thirdPartyFullPath
    case packageType of
        BatchApp -> Shelly.shelly $ Shelly.cmd "mv" unpacked thirdPartyFullPath
        GuiApp   -> Shelly.shelly $ Shelly.cmd "mv" unpacked thirdPartyFullPath
        Lib      -> Shelly.shelly $ Shelly.cmd "mv" unpacked libFullPath




-- createAppimage :: MonadCreatePackage m => Text -> m ()
-- createAppimage appName = do
--     pkgConfig <- get @PackageConfig
--     tmpAppDirPath <- expand $ (pkgConfig ^. defaultPackagePath) </> "appimage" </> convert appName </> convert (appName <> ".AppDir")
--     tmpAppPath <- expand $ (pkgConfig ^. defaultPackagePath) </> "appimage" </> convert appName
--
--     srcPkgPath <- (expand $ (pkgConfig ^. defaultPackagePath) </> convert appName)
--     srcLibPath <- (expand $ (pkgConfig ^. defaultPackagePath) </> convert appName </> "zmq" </> "lib64")
--     let dstPkgPath = (tmpAppDirPath </> "usr" </> "bin")
--         dstLibPath = (tmpAppDirPath </> "usr" </> "lib")
--
--     Shelly.shelly $ Shelly.mkdir_p tmpAppDirPath
--     Shelly.shelly $ Shelly.cd tmpAppPath
--     functions <- downloadWithProgressBar "https://github.com/probonopd/AppImages/raw/master/functions.sh" tmpAppPath
--     Shelly.shelly $ do
--         Shelly.mkdir_p $ tmpAppDirPath </> "usr" </> "bin"
--         Shelly.mkdir_p $ tmpAppDirPath </> "usr" </> "lib"
--
--     Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ ". " ++ (encodeString functions) ++ " && " ++ "get_apprun"
--
--     logoFile <- expand $ (pkgConfig ^. defaultPackagePath) </> convert appName </> convert (pkgConfig ^. studioFolderName) </> convert (pkgConfig ^. studioUtilsFolder) </> convert (pkgConfig ^. logoFileName)
--     desktopFile <- expand $ (pkgConfig ^. defaultPackagePath) </> convert appName </> convert (pkgConfig ^. studioFolderName) </> convert (pkgConfig ^. studioUtilsFolder) </> convert (pkgConfig ^. desktopFileName)
--
--     Shelly.shelly $ do
--         Shelly.cp logoFile $ tmpAppDirPath </> convert (appName <> ".png")
--         Shelly.cp desktopFile $ tmpAppDirPath </> convert (appName <> ".desktop")
--         copyDir srcPkgPath dstPkgPath
--         copyDir srcLibPath dstLibPath
--     appWrapper <- downloadWithProgressBar "https://raw.githubusercontent.com/probonopd/AppImageKit/master/desktopintegration" tmpAppDirPath
--     let dstWrapper = (dstPkgPath </> convert (appName <> ".wrapper"))
--     Shelly.shelly $ Shelly.mv appWrapper dstWrapper
--     makeExecutable dstWrapper
--     Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ "sed -i -e \"s|Exec=" ++ (convert appName) ++ "|Exec=" ++ (convert appName) ++".wrapper|g\" " ++ (convert appName) ++ ".desktop"
--     Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppPath) $ Process.setEnv [("APP", (convert appName))] $ Process.shell $ ". " ++ (encodeString functions) ++ " && " ++ "generate_type2_appimage"

isPathIncluded :: Text -> Text -> Bool
isPathIncluded systemLibPath dylibPath = do
    let dylibSplited = splitDirectories (convert dylibPath)
        systemSplited = splitDirectories (convert systemLibPath)
        l = length systemSplited
        firstL = take l dylibSplited
    (firstL /= systemSplited) && (not $ Filesystem.Path.CurrentOS.null $ convert dylibPath)

install :: MonadIO m=> FilePath -> FilePath -> FilePath -> m ()
install binPath libSystemPath libLocalPath = do
    if filename libLocalPath == filename libSystemPath
        then Shelly.shelly $ Shelly.cmd "install_name_tool" "-change" libSystemPath "@executable_path/../../lib/`basename" binPath
        else return ()

installName :: MonadIO m => FilePath -> FilePath -> FilePath -> m ()
installName binaryPath librariesFolderPath linkedDylib = do
    listedLibrariesFolder <- Shelly.shelly $ Shelly.ls librariesFolderPath
    mapM_ (install binaryPath linkedDylib) listedLibrariesFolder

otool :: MonadIO m => FilePath -> FilePath -> m()
otool libFolderPath binaryPath = do
    deps <- Shelly.shelly $ Shelly.cmd "otool" "-L" binaryPath
    let splited = drop 1 $ Text.strip <$> Text.splitOn "\n" deps
        filePaths = Text.takeWhile (/= ' ') <$> splited
        filtered = convert <$> filter (isPathIncluded "/usr/lib/") filePaths
    mapM_ (installName binaryPath libFolderPath) filtered

linkLibs :: MonadIO m => FilePath -> FilePath -> m ()
linkLibs binPath libPath = Shelly.shelly $ do
    allBins <- Shelly.ls binPath
    mapM_ (otool libPath) allBins

createPkg :: MonadCreatePackage m => ResolvedApplication -> m ()
createPkg resolvedApplication = do
    pkgConfig <- get @PackageConfig

    mapM_ (downloadAndUnpackDependency (convert (resolvedApplication ^. resolvedApp . desc . path))) (resolvedApplication ^. pkgsToPack)
    runBuild $ convert (resolvedApplication ^. resolvedApp . desc . path)
    copyFromRepository (resolvedApplication ^. resolvedApp . header . name) $ convert (resolvedApplication ^. resolvedApp . desc . path)
    mainAppDir <- expand $ (convert (resolvedApplication ^. resolvedApp . desc . path)) </> (pkgConfig ^. defaultPackagePath) </> convert (resolvedApplication ^. resolvedApp . header . name)
    let versionFile = mainAppDir </> (pkgConfig ^. configFolder) </> (pkgConfig ^. versionFileName)
        binsFolder = mainAppDir </> (pkgConfig ^. binFolder) </> (pkgConfig ^. binsPrivate)
        libsFolder = mainAppDir </> (pkgConfig ^. libPath)
    liftIO $ writeFile (encodeString versionFile) $ convert $ showPretty (resolvedApplication ^. resolvedApp . header . version)
    linkLibs binsFolder libsFolder

    case currentHost of
        -- Linux  -> createAppimage (resolvedApplication ^. resolvedApp . header . name)
        Darwin -> do
            Shelly.shelly $ createTarGzUnix mainAppDir (resolvedApplication ^. resolvedApp . header . name)
            return ()








runCreatingPackage :: MonadCreatePackage m => MakePackageOpts -> m ()
runCreatingPackage opts = do
    repo <- parseConfig $ convert (opts ^. Opts.cfgPath)
    let appsToPack = repo ^. apps

    resolved <- mapM (resolvePackageApp repo) appsToPack
    -- print resolved
    mapM_ createPkg resolved


    return ()
