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
        , _buildScriptPath    = "build"
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
        , _versionFileName    = "version.txt"
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
    let depName          = resolvedPackage ^. header . name
        packageType      = resolvedPackage ^. resolvedAppType
        componentsFolder = pkgConfig ^. componentsToCopy

    thirdPartyFullPath <- expand $ repoPath </> componentsFolder </> (pkgConfig ^. thirdPartyPath)
    libFullPath        <- expand $ repoPath </> componentsFolder </> (pkgConfig ^. libPath)
    downloadedPkg      <- downloadFromURL $ resolvedPackage ^. desc . path
    unpacked           <- unpackArchive downloadedPkg
    Shelly.shelly $ Shelly.mkdir_p thirdPartyFullPath
    case packageType of
        BatchApp -> Shelly.shelly $ Shelly.cmd "mv" unpacked thirdPartyFullPath
        GuiApp   -> Shelly.shelly $ Shelly.cmd "mv" unpacked thirdPartyFullPath
        Lib      -> Shelly.shelly $ Shelly.cmd "mv" unpacked libFullPath




createAppimage :: MonadCreatePackage m => Text -> FilePath -> m ()
createAppimage appName repoPath = do
    pkgConfig <- get @PackageConfig
    tmpAppDirPath <- expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> "appimage" </> convert appName </> convert (appName <> ".AppDir")
    tmpAppPath <- expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> "appimage" </> convert appName
    print tmpAppPath
    print tmpAppDirPath
    srcPkgPath <- (expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> convert appName)
    let dstPath = (tmpAppDirPath </> "usr" )


    Shelly.shelly $ Shelly.mkdir_p tmpAppDirPath
    Shelly.shelly $ Shelly.cd tmpAppPath
    functions <- downloadWithProgressBar "https://github.com/probonopd/AppImages/raw/master/functions.sh" tmpAppPath
    let mainAppImageFolder = "usr"
    Shelly.shelly $ do
        Shelly.mkdir_p $ tmpAppDirPath </> mainAppImageFolder

    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ ". " ++ (encodeString functions) ++ " && " ++ "get_apprun"
    mainResources <- expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> convert appName </> (pkgConfig ^. binFolder) </> (pkgConfig ^. binsPublic) </> convert appName </>   (pkgConfig ^. utilsFolder)
    let logoFile = mainResources </> convert (pkgConfig ^. logoFileName)
        desktopFile = mainResources </> convert (pkgConfig ^. desktopFileName)

    Shelly.shelly $ do
        Shelly.cp logoFile $ tmpAppDirPath </> convert (appName <> ".png")
        Shelly.cp desktopFile $ tmpAppDirPath </> convert (appName <> ".desktop")
        copyDir srcPkgPath dstPath
    appWrapper <- downloadWithProgressBar "https://raw.githubusercontent.com/probonopd/AppImageKit/master/desktopintegration" tmpAppDirPath
    let dstWrapperPath = dstPath </> convert (appName <> ".wrapper")
    Shelly.shelly $ Shelly.mv appWrapper dstWrapperPath
    makeExecutable dstWrapperPath
    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ "sed -i -e \"s|Exec=" <> (convert appName) ++ "|Exec=" <> (convert appName) <> ".wrapper|g\" " <> (convert appName) <> ".desktop"
    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppPath) $ Process.setEnv [("APP", (convert appName))] $ Process.shell $ ". " <> (encodeString functions) <> " && " <> "generate_type2_appimage"


-- zrobtenaszewrappery = Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ "sed -i -e \"s|Exec=" ++ (convert appName) ++ "|Exec=" ++ (convert appName) ++".wrapper|g\" " <> desktopFile
--     where desktopFile = convert appName <> ".desktop"

--------------------
--linkingLibsMacOS--
--------------------

isPathIncluded :: Text -> Text -> Bool
isPathIncluded systemLibPath dylibPath = do
    let dylibSplited = splitDirectories $ convert dylibPath
        systemSplited = splitDirectories $ convert systemLibPath
        l = length systemSplited
        firstL = take l dylibSplited
    (firstL /= systemSplited) && (not $ Filesystem.Path.CurrentOS.null $ convert dylibPath)

install :: MonadIO m=> FilePath -> FilePath -> FilePath -> m ()
install binPath libSystemPath libLocalPath = do
    let relativeLibraryPath = "@executable_path/../../lib/`basename"
    if filename libLocalPath == filename libSystemPath
        then Shelly.shelly $ Shelly.cmd "install_name_tool" "-change" libSystemPath relativeLibraryPath binPath
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
        filtered = convert <$> filterSystemLibraries filePaths
        filterSystemLibraries s = filter checkIfSystemLibrary s
        checkIfSystemLibrary = isPathIncluded "/usr/lib/"

    mapM_ (installName binaryPath libFolderPath) filtered

linkLibs :: MonadIO m => FilePath -> FilePath -> m ()
linkLibs binPath libPath = Shelly.shelly $ do
    allBins <- Shelly.ls binPath
    mapM_ (otool libPath) allBins

--------------------
--creatingPackage---
--------------------

createPkg :: MonadCreatePackage m => ResolvedApplication -> m ()
createPkg resolvedApplication = do
    pkgConfig <- get @PackageConfig
    let app        = resolvedApplication ^. resolvedApp
        appDesc    = app ^. desc
        appPath    = appDesc ^. path
        appHeader  = app ^. header
        appName    = appHeader ^. name
        appVersion = appHeader ^. version
    mapM_ (downloadAndUnpackDependency $ convert appPath) $ resolvedApplication ^. pkgsToPack
    runBuild $ convert appPath
    copyFromRepository appName $ convert appPath
    mainAppDir <- expand $ (convert appPath) </> (pkgConfig ^. defaultPackagePath) </> convert appName
    let versionFile = mainAppDir </> (pkgConfig ^. configFolder) </> (pkgConfig ^. versionFileName)
        binsFolder  = mainAppDir </> (pkgConfig ^. binFolder) </> (pkgConfig ^. binsPrivate)
        libsFolder  = mainAppDir </> (pkgConfig ^. libPath)
    liftIO $ writeFile (encodeString versionFile) $ convert $ showPretty appVersion
    case currentHost of
        Linux -> return ()
        Darwin -> linkLibs binsFolder libsFolder

    case currentHost of
        Linux  -> createAppimage appName $ convert appPath
        Darwin -> do
            Shelly.shelly $ createTarGzUnix mainAppDir appName
            return ()



runCreatingPackage :: MonadCreatePackage m => MakePackageOpts -> m ()
runCreatingPackage opts = do
    repo <- parseConfig $ convert (opts ^. Opts.cfgPath)
    let appsToPack = repo ^. apps

    resolved <- mapM (resolvePackageApp repo) appsToPack
    mapM_ createPkg resolved


    return ()
