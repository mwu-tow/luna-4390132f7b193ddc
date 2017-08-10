module Luna.Manager.Command.Install where

import Prologue hiding (txt, FilePath, toText)

import Luna.Manager.System.Host
import Luna.Manager.System.Env
import Luna.Manager.Component.Repository as Repo
import Luna.Manager.Component.Version
import Luna.Manager.Network
import Luna.Manager.Component.Pretty
import Luna.Manager.Shell.Question
import           Luna.Manager.Command.Options (InstallOpts)
import qualified Luna.Manager.Command.Options as Opts
import Luna.Manager.System.Path
import Luna.Manager.System (makeExecutable, exportPath', checkShell, runServicesWindows, stopServicesWindows, exportPathWindows)

import Control.Lens.Aeson
import Control.Monad.Raise
import Control.Monad.State.Layered

import qualified Data.Char as Char
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import qualified Data.Text as Text


import qualified Data.Yaml as Yaml

import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, decodeString, toText, basename, hasExtension, parent)
import Luna.Manager.Shell.Shelly (toTextIgnore, MonadSh)
import qualified Luna.Manager.Shell.Shelly as Shelly
import System.IO (hFlush, stdout)
import qualified System.Process.Typed as Process
import qualified System.Directory as System
import qualified System.Environment as Environment
import Luna.Manager.Archive as Archive


-- FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
-- FIXME: Remove it as fast as we upload config yaml to the server
-- hardcodedRepo :: Repo
-- hardcodedRepo = Repo defpkgs ["studio"] where
--     defpkgs = mempty & at "lib1"         .~ Just (Package "lib1 synopsis"     $ fromList [ (Version 1 0 0 Nothing      , fromList [(SysDesc Linux X64, PackageDesc mempty "path")] )])
--                      & at "luna-studio"  .~ Just (Package "studio synopsis"   $ fromList [ (Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
--                                                                                          , (Version 1 0 0 (Just $ RC 6), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
--                                                                                          , (Version 1 1 0 Nothing      , fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
--                                                                                          ])
--
--                      & at "luna"         .~ Just (Package "compiler synopsis" $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 (Just $ RC 5))] "path")] )])
--                      & at "luna-manager" .~ Just (Package "manager synopsis"  $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 (Just $ RC 5))] "path")] )])



---------------------------------
-- === Installation config === --
---------------------------------

-- === Definition === --

data InstallConfig = InstallConfig { _defaultConfPath        :: FilePath
                                   , _defaultBinPathBatchApp :: FilePath
                                   , _defaultBinPathGuiApp   :: FilePath
                                   , _localName              :: Text
                                   , _selectedVersionPath    :: FilePath
                                   , _mainBinPath            :: FilePath
                                   , _resourcesPath          :: FilePath
                                   , _localBinPath           :: FilePath
                                   , _logoFileName           :: FilePath
                                   , _infoFileName           :: FilePath
                                   , _libPath                :: FilePath
                                   , _privateBinPath         :: FilePath
                                   , _configPath             :: FilePath
                                   , _logsFolder             :: FilePath
                                   , _thirdParty             :: FilePath
                                   }
makeLenses ''InstallConfig


-- === Instances === --


instance Monad m => MonadHostConfig InstallConfig 'Linux arch m where
    defaultHostConfig = return $ InstallConfig
        { _defaultConfPath         = "~/.luna"
        , _defaultBinPathBatchApp  = "~/.luna/bin"
        , _defaultBinPathGuiApp    = "~/.luna/bin"
        , _localName               = "local"
        , _selectedVersionPath     = "current"
        , _mainBinPath             = "bin/main"
        , _resourcesPath           = "resources"
        , _localBinPath            = "~/.local/bin"
        , _logoFileName            = "logo.png"
        , _infoFileName            = "app.desktop"
        , _libPath                 = "lib"
        , _privateBinPath          = "bin/private"
        , _configPath              = "config"
        , _logsFolder              = "logs"
        , _thirdParty              = "third-party"
        }

instance Monad m => MonadHostConfig InstallConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & defaultBinPathGuiApp   .~ "/Applications"
                           & localBinPath           .~ "/usr/local/bin"
                           & logoFileName           .~ "logo.icns"
                           & infoFileName           .~ "Info.plist"

instance Monad m => MonadHostConfig InstallConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & defaultBinPathGuiApp   .~ "C:\\Program Files"



-----------------------
-- === Installer === --
-----------------------

-- === Errors === --

newtype UnresolvedDepsError = UnresolvedDepsError [PackageHeader] deriving (Show)
makeLenses ''UnresolvedDepsError

instance Exception UnresolvedDepsError where
    displayException err = "Following dependencies were unable to be resolved: " <> show (showPretty <$> unwrap err)

type MonadInstall m = (MonadStates '[EnvConfig, InstallConfig, RepoConfig] m, MonadNetwork m, Shelly.MonadSh m, Shelly.MonadShControl m) 


-- === Utils === --

mkSystemPkgName :: Text -> Text
mkSystemPkgName = case currentHost of
    Linux   -> id
    Darwin  -> mkCamelCaseName
    Windows -> mkCamelCaseName

mkCamelCaseName :: Text -> Text
mkCamelCaseName txt = convert $ goHead (convert txt) where
    goHead :: [Char] -> [Char]
    goHead = \case []     -> []
                   (s:ss) -> Char.toUpper s : goBody ss
    goBody = \case []       -> []
                   ('-':ss) -> goHead ss
                   (s  :ss) -> s : goBody ss

prepareInstallPath :: MonadInstall m => AppType -> FilePath -> Text -> Text -> m FilePath
prepareInstallPath appType appPath appName appVersion = expand $ case currentHost of
    Linux   -> appPath </> convert (mkSystemPkgName appName) </> convert appVersion
    Windows -> case appType of
        GuiApp -> appPath </> convert (mkSystemPkgName appName) </> convert appVersion
        BatchApp -> appPath </> convert appName </> convert appVersion
    Darwin  -> case appType of
        GuiApp   -> appPath </> convert ((mkSystemPkgName appName) <> ".app") </> "Contents" </> "Resources" </> convert appVersion
        BatchApp -> appPath </> convert (mkSystemPkgName appName) </> convert appVersion

downloadAndUnpackApp :: MonadInstall m => URIPath -> FilePath -> Text -> m ()
downloadAndUnpackApp pkgPath installPath appName = do
    testInstallPath <- Shelly.test_d installPath
    if testInstallPath then Shelly.rm_rf installPath else return ()

    Shelly.mkdir_p $ parent installPath
    pkg <- downloadWithProgressBar pkgPath
    unpacked <- Archive.unpack pkg
    case currentHost of
         Linux -> do
             Shelly.mkdir_p installPath
             Shelly.mv unpacked  $ installPath </> convert appName
         Darwin -> Shelly.mv unpacked  installPath
         Windows -> Shelly.mv unpacked  installPath
    -- Shelly.rm_rf tmp -- FIXME[WD -> SB]: I commented it out, we use downloadWithProgressBar now which automatically downloads to tmp.
                        --                  However, manuall tmp removing is error prone! Create a wrapper like `withTmp $ \tmp -> downloadWithProgressBarTo pkgPath tmp; ...`
                        --                  which automatically removes tmp on the end!

linkingCurrent :: MonadInstall m => AppType -> FilePath -> m ()
linkingCurrent appType installPath = do
    installConfig <- get @InstallConfig
    let currentPath = (parent installPath) </> (installConfig ^. selectedVersionPath)
    createSymLinkDirectory installPath currentPath

makeShortcuts :: MonadInstall m => FilePath -> Text -> m ()
makeShortcuts packageBinPath appName = case currentHost of
    Windows -> do
        bin <- liftIO $ System.getSymbolicLinkTarget $ encodeString packageBinPath
        binAbsPath <- Shelly.canonicalize $ (parent packageBinPath) </> (decodeString bin)
        userProfile <- liftIO $ Environment.getEnv "userprofile"
        let menuPrograms = (decodeString userProfile) </> "AppData" </> "Roaming" </> "Microsoft" </> "Windows" </> "Start Menu" </> "Programs" </> convert (appName <> ".lnk")
        liftIO $ Process.runProcess_ $ Process.shell ("powershell" <> " \"$s=New-Object -ComObject WScript.Shell; $sc=$s.createShortcut(" <> "\'" <> (encodeString menuPrograms) <> "\'" <> ");$sc.TargetPath=" <> "\'" <> (encodeString binAbsPath) <> "\'" <> ";$sc.Save()\"" )
        exportPathWindows packageBinPath
    otherwise -> return ()

postInstallation :: MonadInstall m => AppType -> FilePath -> Text -> Text -> Text -> m ()
postInstallation appType installPath binPath appName version = do
    linkingCurrent appType installPath
    installConfig <- get @InstallConfig
    packageBin <- case currentHost of
        Linux   -> return $ installPath </> convert appName
        Darwin  -> return $ installPath </> (installConfig ^. mainBinPath) </> convert appName
        Windows -> return $ installPath </> (installConfig ^. mainBinPath) </> convert (appName <> ".exe")
    currentBin <- case currentHost of
        Linux -> return $ parent installPath </> (installConfig ^. selectedVersionPath)  </> convert (mkSystemPkgName appName)
        Darwin -> case appType of
            --TODO[1.1] lets think about making it in config
            GuiApp   -> expand $ convert binPath </> convert ((mkSystemPkgName appName) <> ".app") </> "Contents" </> "MacOS" </> convert (mkSystemPkgName appName)
            BatchApp -> return $ parent installPath </> (installConfig ^. selectedVersionPath) </> convert (mkSystemPkgName appName)
        Windows -> case appType of
            BatchApp -> return $ parent installPath </> (installConfig ^. selectedVersionPath) </> convert (appName <> ".exe")
            GuiApp   -> return $ parent installPath </> (installConfig ^. selectedVersionPath) </> convert (mkSystemPkgName appName) </> (installConfig ^. selectedVersionPath) </> convert (appName <> ".exe")
    makeExecutable packageBin
    case currentHost of
        Linux -> linkingLocalBin currentBin appName
        Darwin -> case appType of
            GuiApp -> do
                linking packageBin currentBin
                linkingLocalBin currentBin appName
            BatchApp -> linkingLocalBin currentBin appName
        Windows -> return () --linking packageBin currentBin

    copyResources appType installPath appName
    runServices installPath appType appName version
    makeShortcuts packageBin appName

copyResources :: MonadInstall m => AppType -> FilePath -> Text -> m ()
copyResources appType installPath appName = case currentHost of
    Linux -> return ()
    Darwin -> case appType of
        GuiApp -> do
            installConfig <- get @InstallConfig
            let resources        = installPath </> (installConfig ^. mainBinPath) </> (installConfig ^. resourcesPath)
                packageLogo      = resources </> (installConfig ^. logoFileName)
                packageInfoPlist = resources </> (installConfig ^. infoFileName)
                appLogo          = parent installPath </> convert (appName <> ".icns")
                -- appInfoPlist     = (parent $ parent installPath) </> "Info.plist"
            -- Shelly.rm appLogo
            Shelly.cp packageLogo appLogo
            -- Shelly.rm appInfoPlist
            Shelly.cp packageInfoPlist (parent $ parent installPath)
        BatchApp -> return ()
    Windows -> return ()

linking :: MonadInstall m => FilePath -> FilePath -> m ()
linking src dst = do
    Shelly.mkdir_p $ parent dst
    createSymLink src dst

linkingLocalBin :: (MonadInstall m, MonadIO m) => FilePath -> Text -> m ()
linkingLocalBin currentBin appName = do
    home <- getHomePath
    installConfig <- get @InstallConfig
    case currentHost of
        Linux -> do
            localBin <- expand $ (installConfig ^. localBinPath) </> convert (mkSystemPkgName appName)
            linking currentBin localBin
            exportPath' localBin
        Darwin -> do
            localBin <- expand $ (installConfig ^. localBinPath) </> convert appName
            linking currentBin localBin
        Windows -> exportPath' currentBin

-----Windows specific-----

stopServices ::MonadInstall m => FilePath -> AppType -> m ()
stopServices installPath appType = case currentHost of
    Windows -> case appType of
        GuiApp -> do
            installConfig<- get @InstallConfig
            let currentServices = parent installPath </> (installConfig ^. selectedVersionPath) </> (installConfig ^. configPath) </> fromText "windows"
            do
                testservices <- Shelly.test_d currentServices
                if testservices then stopServicesWindows currentServices else return ()
        BatchApp -> return ()
    otherwise -> return ()


runServices :: MonadInstall m => FilePath -> AppType -> Text -> Text -> m ()
runServices installPath appType appName version = case currentHost of
    Windows -> case appType of
        GuiApp -> do
            installConfig <- get @InstallConfig
            let services = installPath </> (installConfig ^. configPath) </> fromText "windows"
            logs <- expand $ (installConfig ^. defaultConfPath) </> (installConfig ^. logsFolder) </> fromText appName </> (fromText $ showPretty version)
            runServicesWindows services logs
        BatchApp -> return ()
    otherwise -> return ()

-- whenHost :: MonadInstall m => forall system a. m a -> m ()
-- whenHost f = when (currentHost == fromType @a) (void f) --TODO : use for matching on single host + refactor -> mv function to utils

copyDllFilesOnWindows :: MonadInstall m => FilePath -> m ()
copyDllFilesOnWindows installPath = case currentHost of -- whenHost @'Windows $ do
    Windows -> do
        installConfig <- get @InstallConfig
        let libFolderPath = installPath </> (installConfig ^. libPath)
            binsFolderPath = installPath </> (installConfig ^. privateBinPath)
        do
            listedLibs <- Shelly.ls libFolderPath
            mapM_ (`Shelly.mv` binsFolderPath) listedLibs
    otherwise -> return ()

copyWinSW :: MonadInstall m => FilePath -> m ()
copyWinSW installPath = case currentHost of
    Linux -> return ()
    Darwin -> return ()
    Windows -> do
        installConfig <- get @InstallConfig
        let winSW = installPath </> (installConfig ^. thirdParty) </> fromText "WinSW.Net4.exe"
            winConfigFolderPath = installPath </> (installConfig ^. configPath) </> fromText "windows"
        Shelly.mv winSW winConfigFolderPath

prepareWindowsPkgForRunning :: MonadInstall m => FilePath -> m ()
prepareWindowsPkgForRunning installPath = do
    copyDllFilesOnWindows installPath
    copyWinSW installPath

-----InstallationUtils------

askLocation :: (MonadStates '[EnvConfig, InstallConfig, RepoConfig] m, MonadNetwork m) => InstallOpts -> AppType -> Text -> m Text
askLocation opts appType appName = do
    installConfig <- get @InstallConfig
    let pkgInstallDefPath = case appType of
            GuiApp   -> installConfig ^. defaultBinPathGuiApp
            BatchApp -> installConfig ^. defaultBinPathBatchApp
    binPath <- askOrUse (opts ^. Opts.selectedInstallationPath)
        $ question ("Select installation path for " <> appName) plainTextReader
        & defArg .~ Just (toTextIgnore pkgInstallDefPath) --TODO uzyć toText i złapać tryRight'
    return binPath

installApp :: MonadInstall m => InstallOpts -> ResolvedPackage -> m ()
installApp opts package = do
    let pkgName    = package ^. header . name
        appType    = package ^. resolvedAppType
        pkgVersion = showPretty $ package ^. header . version
    binPath     <- askLocation opts appType pkgName

    installPath <- prepareInstallPath appType (convert binPath) pkgName $ pkgVersion
    stopServices installPath appType
    pathExists  <- Shelly.test_d installPath
    if pathExists then Shelly.rm_rf installPath else return ()
    downloadAndUnpackApp (package ^. desc . path) installPath pkgName
    prepareWindowsPkgForRunning installPath
    postInstallation appType installPath binPath pkgName pkgVersion

data VersionError = VersionError deriving (Show)
instance Exception VersionError where
    displayException err = "Incorrect version: " <> show err

versionError :: SomeException
versionError = toException VersionError

readVersion :: (MonadIO m, MonadException SomeException m) => Text -> m Version
readVersion v = case readPretty v of
    Left e -> raise versionError
    Right v -> return $ v

-- === Running === --

run :: MonadInstall m => InstallOpts -> m ()
run opts = do
    repo <- getRepo

    (appName, appPkg) <- askOrUse (opts ^. Opts.selectedComponent)
        $ question "Select component to be installed" (\t -> choiceValidator' "component" t $ (t,) <$> Map.lookup t (repo ^. packages))
        & help   .~ choiceHelp "components" (repo ^. apps)
        & defArg .~ maybeHead (repo ^. apps)

    let vmap = Map.mapMaybe (Map.lookup currentSysDesc) $ appPkg ^. versions
        vss  = sort . Map.keys $ vmap
    (appVersion, appPkgDesc) <- askOrUse (opts ^. Opts.selectedVersion)
        $ question "Select version to be installed" (\t -> choiceValidator "version" t . sequence $ fmap (t,) . flip Map.lookup vmap <$> readPretty t)
        & help   .~ choiceHelp (appName <> " versions") vss
        & defArg .~ fmap showPretty (maybeLast vss)

    let (unresolvedLibs, pkgsToInstall) = Repo.resolve repo appPkgDesc
    when (not $ null unresolvedLibs) . raise' $ UnresolvedDepsError unresolvedLibs

    version <- readVersion appVersion
    let appsToInstall = filter (( <$> (^. header . name)) (`elem` (repo ^.apps))) pkgsToInstall
        resolvedApp = ResolvedPackage (PackageHeader appName version) appPkgDesc (appPkg ^. appType)
        allApps = resolvedApp : appsToInstall
    mapM_ (installApp opts) $ allApps

    -- print $ "TODO: Install the libs (each with separate progress bar): " <> show pkgsToInstall -- w ogóle nie supportujemy przeciez instalowania osobnych komponentów i libów
    -- print $ "TODO: Add new exports to bashRC if not already present"
    -- print $ "TODO: IMPORTANT: be sure that installation of manager updates the manager in-place"
    -- TODO: powinnismy zrobic funckje "installPackage" i przemapowac ja przez app i pkgsToInstall
    --       i to powinien byc koniec "instalacji" - potem jeszcze dopisywanie do shelli sciezek etc



    -- return ()
