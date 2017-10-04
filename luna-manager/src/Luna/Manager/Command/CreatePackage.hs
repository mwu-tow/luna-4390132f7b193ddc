{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Command.CreatePackage where

import           Control.Lens.Aeson
import           Control.Monad.Raise
import           Control.Monad.State.Layered
import           Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, decodeString, parent, splitDirectories, null, filename)
import           Luna.Manager.Archive as Archive
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
import qualified Luna.Manager.Shell.Shelly as Shelly
import qualified System.Process.Typed as Process
import System.Exit
import System.Directory (renameDirectory)
import Luna.Manager.Shell.Shelly (MonadSh)
import qualified Control.Exception.Safe as Exception
import qualified Data.ByteString.Lazy.Char8 as BSLChar

----------------------------
-- === Package config === --
----------------------------

-- === Definition === --

data PackageConfig = PackageConfig { _defaultPackagePath     :: FilePath
                                   , _buildScriptPath        :: FilePath
                                   , _thirdPartyPath         :: FilePath
                                   , _libPath                :: FilePath
                                   , _componentsToCopy       :: FilePath
                                   , _configFolder           :: FilePath
                                   , _binFolder              :: FilePath
                                   , _binsPrivate            :: FilePath
                                   , _mainBin                :: FilePath
                                   , _utilsFolder            :: FilePath
                                   , _logoFileName           :: Text
                                   , _desktopFileName        :: Text
                                   , _versionFileName        :: FilePath
                                   }

makeLenses ''PackageConfig

type MonadCreatePackage m = (MonadStates '[EnvConfig, PackageConfig, RepoConfig] m, MonadNetwork m, MonadSh m, Shelly.MonadShControl m)


-- === Instances === --

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
        , _mainBin            = "main"
        , _utilsFolder        = "resources"
        , _logoFileName       = "logo.svg"
        , _desktopFileName    = "app.desktop"
        , _versionFileName    = "version.txt"
        }

instance Monad m => MonadHostConfig PackageConfig 'Darwin arch m where
    defaultHostConfig = defaultHostConfigFor @Linux

instance Monad m => MonadHostConfig PackageConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & buildScriptPath .~ "build.bat"
                           & defaultPackagePath .~ "C:\\tmp\\luna-package"

data AppimageException = AppimageException SomeException deriving (Show)
instance Exception AppimageException where
   displayException (AppimageException exception ) = "AppImage not created because of: " <> displayException exception

data ExistingVersionException = ExistingVersionException Version deriving (Show)
instance Exception ExistingVersionException where
  displayException (ExistingVersionException v) = "This version already exists: " <> (convert $ showPretty v)

----------------------
-- === Appimage === --
----------------------

-- zlintuj
modifyDesktopFileToUseWrapperAppImageToRunApp :: MonadCreatePackage m => Text -> FilePath -> m ()
modifyDesktopFileToUseWrapperAppImageToRunApp appName tmpAppDirPath =
    Process.runProcess_ $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ inPlaceSubs <> substitute <> desktopFile
    where desktopFile     = convert appName <> ".desktop"
          wrappedExecName = convert appName <> ".wrapper"
          substitute      = "\"s|Exec=" <> (convert appName) <> "|Exec=" <> wrappedExecName <> "|g\" "
          inPlaceSubs     = "sed -i -e "

copyResourcesAppImage :: MonadCreatePackage m => FilePath -> Text -> FilePath -> FilePath -> m ()
copyResourcesAppImage repoPath appName tmpAppDirPath mainAppImageFolderPath = do
    pkgConfig  <- get @PackageConfig
    srcPkgPath <- expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
    let utilsPath   = srcPkgPath </> (pkgConfig ^. binFolder) </> (pkgConfig ^. mainBin) </> (pkgConfig ^. utilsFolder)
        logoFile    = utilsPath </> convert (pkgConfig ^. logoFileName)
        desktopFile = utilsPath </> convert (pkgConfig ^. desktopFileName)
    Shelly.cp logoFile    $ tmpAppDirPath </> convert (appName <> ".svg")
    Shelly.cp desktopFile $ tmpAppDirPath </> convert (appName <> ".desktop")
    copyDir srcPkgPath mainAppImageFolderPath

checkAppImageName :: MonadCreatePackage m => Text -> FilePath -> m ()
checkAppImageName appName filePath = do
    let fileName = filename filePath
        outFolderPath = parent $ filePath
    if Text.isInfixOf appName (Shelly.toTextIgnore fileName)
        then do
            Shelly.mv filePath $ outFolderPath </> convert (appName <> ".AppImage")
            else return ()

changeAppImageName :: MonadCreatePackage m => Text -> FilePath -> m ()
changeAppImageName appName outFolderPath = do
    listedDir <- Shelly.ls outFolderPath
    mapM_ (checkAppImageName appName) listedDir

getApprun :: MonadCreatePackage m => FilePath -> FilePath -> m ()
getApprun tmpAppDirPath functions = do
    let apprun = "get_apprun"
    (exitCode, out, err) <- Process.readProcess $ Process.setWorkingDir (encodeString tmpAppDirPath) $ Process.shell $ ". " <> (encodeString functions) <> " && " <> apprun
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure a -> throwM (AppimageException (toException $ Exception.StringException (BSLChar.unpack err) callStack ))

generateAppimage :: MonadCreatePackage m => FilePath -> FilePath -> Text -> m ()
generateAppimage tmpAppPath functions appName = do
    let generateAppimage   = "generate_type2_appimage"
    (exitCode, out, err) <- Process.readProcess $ Process.setWorkingDir (encodeString tmpAppPath) $ Process.setEnv [("APP", (convert appName))] $ Process.shell $ ". " <> (encodeString functions) <> " && " <> generateAppimage
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure a -> throwM (AppimageException (toException $ Exception.StringException (BSLChar.unpack err) callStack ))

createAppimage :: MonadCreatePackage m => Text -> FilePath -> m ()
createAppimage appName repoPath = do
    let appImageFolderName = "appimage"
    pkgConfig     <- get @PackageConfig
    tmpAppPath    <- expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> appImageFolderName </> convert appName
    let tmpAppDirPath = tmpAppPath </> convert (appName <> ".AppDir")

    Shelly.mkdir_p tmpAppDirPath

    putStrLn "Downloading AppImage functions.sh"
    functions <- downloadWithProgressBarTo "https://github.com/probonopd/AppImages/raw/master/functions.sh" tmpAppPath False
    let mainAppImageFolder     = "usr"
        mainAppImageFolderPath = tmpAppDirPath </> mainAppImageFolder
    Shelly.mkdir_p mainAppImageFolderPath

    getApprun tmpAppDirPath functions
    copyResourcesAppImage repoPath appName tmpAppDirPath mainAppImageFolderPath

    putStrLn "Downloading AppImage desktopIntegration"
    appWrapper <- downloadWithProgressBarTo "https://raw.githubusercontent.com/probonopd/AppImageKit/master/desktopintegration" tmpAppDirPath False
    let dstWrapperPath = mainAppImageFolderPath </> convert (appName <> ".wrapper")
    Shelly.mv appWrapper dstWrapperPath
    makeExecutable dstWrapperPath
    modifyDesktopFileToUseWrapperAppImageToRunApp appName tmpAppDirPath

    generateAppimage tmpAppPath functions appName

    let outFolder = (parent $ tmpAppPath) </> "out"
    changeAppImageName appName outFolder

------------------------------
-- === Package building === --
------------------------------

-- === Utils === --


runPkgBuildScript :: MonadCreatePackage m => Bool -> FilePath -> m ()
runPkgBuildScript verbose repoPath = do
    pkgConfig <- get @PackageConfig
    buildPath <- expand $ repoPath </> (pkgConfig ^. buildScriptPath)
    Shelly.chdir (parent buildPath) $ do
        if verbose then Shelly.cmd buildPath "--release" else Shelly.silently $ Shelly.cmd buildPath "--release"

copyFromDistToDistPkg :: MonadCreatePackage m => Text -> FilePath -> m ()
copyFromDistToDistPkg appName repoPath = do
    pkgConfig         <- get @PackageConfig
    packageRepoFolder <- case currentHost of
        Darwin  -> expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
        Linux   -> expand $ repoPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
        Windows -> return $ (pkgConfig ^. defaultPackagePath) </> convert appName
    let expandedCopmponents = repoPath </> (pkgConfig ^. componentsToCopy)
    Shelly.rm_rf packageRepoFolder
    Shelly.mkdir_p $ parent packageRepoFolder
    Shelly.mv expandedCopmponents packageRepoFolder

downloadAndUnpackDependency :: MonadCreatePackage m => FilePath -> ResolvedPackage -> m ()
downloadAndUnpackDependency repoPath resolvedPackage = do
    pkgConfig <- get @PackageConfig
    let depName          = resolvedPackage ^. header . name
        packageType      = resolvedPackage ^. resolvedAppType
        componentsFolder = pkgConfig ^. componentsToCopy
        guiInstaller     = False
    thirdPartyFullPath <- expand $ repoPath </> componentsFolder </> (pkgConfig ^. thirdPartyPath)
    libFullPath        <- expand $ repoPath </> componentsFolder </> (pkgConfig ^. libPath)
    downloadedPkg      <- downloadFromURL guiInstaller (resolvedPackage ^. desc . path) $ "Downloading dependency files " <> depName
    unpacked           <- Archive.unpack guiInstaller 1.0 "unpacking_progress" downloadedPkg
    Shelly.rm_rf thirdPartyFullPath
    Shelly.mkdir_p thirdPartyFullPath
    case packageType of
        BatchApp -> Shelly.mv unpacked thirdPartyFullPath
        GuiApp   -> Shelly.mv unpacked thirdPartyFullPath
        Lib      -> do
            unpackedIsDir <- Shelly.test_d unpacked
            if unpackedIsDir then do
                listed <- Shelly.ls unpacked
                if length listed == 1 then do
                    listedIsDir <- Shelly.test_d $ head listed
                    if listedIsDir then
                        mapM_ (flip Shelly.mv libFullPath) listed
                        else Shelly.mv unpacked libFullPath
                    else Shelly.mv unpacked libFullPath
                else Shelly.mv unpacked libFullPath

isNewestVersion :: MonadCreatePackage m => Version -> Text -> m Bool
isNewestVersion appVersion appName = do
    repo <- getRepo True
    versionList <- Repo.getVersionsList repo appName
    if (head versionList) >= appVersion then return False else return True

------------------------------
-- === linkingLibsMacOS === --
------------------------------

isSubPath :: Text -> Text -> Bool
isSubPath systemLibPath dylibPath = do
    let dylibSplited  = splitDirectories $ convert dylibPath
        systemSplited = splitDirectories $ convert systemLibPath
        l             = length systemSplited
        firstL        = take l dylibSplited
    (firstL /= systemSplited) && (not $ Filesystem.Path.CurrentOS.null $ convert dylibPath)

changeExecutableLibPathToRelative :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => FilePath -> FilePath -> FilePath -> m ()
changeExecutableLibPathToRelative binPath libSystemPath libLocalPath = do
    let dylibName           = filename libSystemPath
        relativeLibraryPath = "@executable_path/../../lib/" <> Shelly.toTextIgnore dylibName
        binFolder           = parent binPath
        binName             = "./"  <> (Shelly.toTextIgnore $ filename binPath)

    if filename libLocalPath == filename libSystemPath
        then do
            Shelly.chdir binFolder $ do
                Shelly.cmd "install_name_tool" "-change" libSystemPath relativeLibraryPath binName
        else return ()

changeExecutablesLibPaths :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => FilePath -> FilePath -> FilePath -> m ()
changeExecutablesLibPaths binaryPath librariesFolderPath linkedDylib = do
    listedLibrariesFolder <- Shelly.ls librariesFolderPath
    mapM_ (changeExecutableLibPathToRelative binaryPath linkedDylib) listedLibrariesFolder


checkAndChangeExecutablesLibPaths :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => FilePath -> FilePath -> m()
checkAndChangeExecutablesLibPaths libFolderPath binaryPath = do
    deps <- Shelly.cmd "otool" "-L" binaryPath
    let splited                 = drop 1 $ Text.strip <$> Text.splitOn "\n" deps
        filePaths               = Text.takeWhile (/= ' ') <$> splited
        filtered                = convert <$> filterSystemLibraries filePaths
        filterSystemLibraries s = filter checkIfSystemLibrary s
        checkIfSystemLibrary    = isSubPath "/usr/lib/"

    mapM_ (changeExecutablesLibPaths binaryPath libFolderPath) filtered

linkLibs :: (MonadIO m, MonadSh m, Shelly.MonadShControl m) => FilePath -> FilePath -> m ()
linkLibs binPath libPath = do
    allBins <- Shelly.ls binPath
    mapM_ (checkAndChangeExecutablesLibPaths libPath) (filterGitKeepFile allBins)

filterGitKeepFile :: [FilePath] -> [FilePath]
filterGitKeepFile allBins = filter (\x -> filename x /= ".gitkeep") allBins

-------------------------------
-- === Creating package === ---
-------------------------------

createPkg :: MonadCreatePackage m => Bool -> FilePath -> ResolvedApplication -> m ()
createPkg verbose cfgFolderPath resolvedApplication = do
    pkgConfig <- get @PackageConfig
    let app        = resolvedApplication ^. resolvedApp
        appDesc    = app ^. desc
        appPath    = if (appDesc ^. path) == "./" then cfgFolderPath else convert (appDesc ^. path)
        appHeader  = app ^. header
        appName    = appHeader ^. name
        appType    = app ^. resolvedAppType
    appVersion <- do
        isNewest <- isNewestVersion (appHeader ^. version) appName
        if isNewest then return (appHeader ^. version) else throwM $ ExistingVersionException (appHeader ^. version)
    mapM_ (downloadAndUnpackDependency appPath) $ resolvedApplication ^. pkgsToPack
    runPkgBuildScript verbose appPath
    copyFromDistToDistPkg appName appPath
    mainAppDir <- case currentHost of
        Linux   -> expand $ appPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
        Darwin  -> expand $ appPath </> (pkgConfig ^. defaultPackagePath) </> convert appName
        Windows -> return $ (pkgConfig ^. defaultPackagePath) </> convert appName
    let versionFile = mainAppDir </> (pkgConfig ^. configFolder) </> (pkgConfig ^. versionFileName)
        binsFolder  = mainAppDir </> (pkgConfig ^. binFolder) </> (pkgConfig ^. binsPrivate)
        libsFolder  = mainAppDir </> (pkgConfig ^. libPath)

    Shelly.mkdir_p $ parent versionFile
    liftIO $ writeFile (encodeString versionFile) $ convert $ showPretty appVersion
    case currentHost of
        Linux   -> return ()
        Darwin  -> Shelly.silently $ linkLibs binsFolder libsFolder
        Windows -> return ()

    case currentHost of
        Linux   -> createAppimage appName $ appPath
        Darwin  -> void $ createTarGzUnix mainAppDir appName
        Windows -> void $ zipFileWindows False mainAppDir appName

updateConfig :: Repo -> ResolvedApplication -> Repo
updateConfig config resolvedApplication =
    let app        = resolvedApplication ^. resolvedApp
        appDesc    = app ^. desc
        -- appPath    = convert (appDesc ^. path)
        appHeader  = app ^. header
        appName    = appHeader ^. name
        mainPackagePath = "https://s3-us-west-2.amazonaws.com/packages-luna/"
        applicationPartPackagePath = appName <> "/" <> showPretty (view version appHeader) <> "/" <> appName
        s3Path = case currentHost of
            Darwin -> mainPackagePath <> "darwin/" <> applicationPartPackagePath <> ".tar.gz"
            Linux -> mainPackagePath <> "linux/" <> applicationPartPackagePath <> ".AppImage"
            Windows -> mainPackagePath <> "windows/" <> applicationPartPackagePath <> ".tar.gz"
        updatedConfig = config & packages . ix appName . versions . ix (view version appHeader) . ix currentSysDesc . path .~ s3Path
        filteredConfig = updatedConfig & packages . ix appName . versions . ix (view version appHeader)  %~ Map.filterWithKey (\k _ -> k == currentSysDesc   )
    in filteredConfig

run :: MonadCreatePackage m => MakePackageOpts -> m ()
run opts = do
    config <- parseConfig $ convert (opts ^. Opts.cfgPath)
    let cfgFolderPath = parent $ convert (opts ^. Opts.cfgPath)
        appsToPack = config ^. apps

    resolved <- mapM (resolvePackageApp config) appsToPack

    mapM_ (createPkg (opts ^. Opts.verbose) cfgFolderPath) resolved
    repo <- getRepo False
    let updatedConfig = foldl' updateConfig config resolved
    generateConfigYamlWithNewPackage repo updatedConfig $ cfgFolderPath </> "config.yaml"
