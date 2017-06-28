module Luna.Manager.Command.Install where

import Prologue hiding (txt, FilePath, toText, fromText)

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
import Luna.Manager.System (makeExecutable, exportPath, checkShell, runServicesWindows)

import Control.Lens.Aeson
import Control.Monad.Raise
import Control.Monad.State.Layered

import qualified Data.Map as Map

import qualified Data.Yaml as Yaml

import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, fromText, basename)
import Shelly.Lifted (toTextIgnore)
import qualified Shelly.Lifted as Shelly
import Luna.Manager.Archive
import Luna.Manager.Component.WindowsInstallationUtils

---------------------------------
-- === Installation config === --
---------------------------------

-- === Definition === --

data InstallConfig = InstallConfig { _execName        :: Text
                                   , _defaultConfPath :: FilePath
                                   , _defaultBinPath  :: FilePath
                                   , _localName       :: Text
                                   }
makeLenses ''InstallConfig


-- === Instances === --

instance Monad m => MonadHostConfig InstallConfig 'Linux arch m where
    defaultHostConfig = return $ InstallConfig
        { _execName        = "luna-studio"
        , _defaultConfPath = "~/.luna"
        , _defaultBinPath  = "~/.luna-bin" --TODO jak tylko bedzie poprawne expandowanie tyldy to przywrócic stara wersje
        , _localName       = "local"
        }

instance Monad m => MonadHostConfig InstallConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & execName       .~ "LunaStudio"
                           & defaultBinPath .~ "~/Applications"

instance Monad m => MonadHostConfig InstallConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & execName       .~ "LunaStudio"
                           & defaultBinPath .~ "C:\\ProgramFiles"



-----------------------
-- === Installer === --
-----------------------

-- === Errors === --

newtype UnresolvedDepsError = UnresolvedDepsError [PackageHeader] deriving (Show)
makeLenses ''UnresolvedDepsError

instance Exception UnresolvedDepsError where
    displayException err = "Following dependencies were unable to be resolved: " <> show (showPretty <$> unwrap err)


-- === Running === --

type MonadInstall m = (MonadStates '[EnvConfig, InstallConfig, RepoConfig] m, MonadNetwork m)

runInstaller :: MonadInstall m => InstallOpts -> m ()
runInstaller opts = do
    repo <- getRepo
    print $ show (Map.keys $ repo ^. apps)
    (appName, appPkg) <- askOrUse (opts ^. Opts.selectedComponent)
        $ question "Select component to be installed" (\t -> choiceValidator' "component" t $ (t,) <$> Map.lookup t (repo ^. apps))
        & help   .~ choiceHelp "components" (Map.keys $ repo ^. apps)
        & defArg .~ Just (repo ^. defaultApp)

    let vmap = Map.mapMaybe (Map.lookup currentSysDesc) $ appPkg ^. versions
        vss  = sort . Map.keys $ vmap
    (appVersion, appPkgDesc) <- askOrUse (opts ^. Opts.selectedVersion)
        $ question "Select version to be installed" (\t -> choiceValidator "version" t . sequence $ fmap (t,) . flip Map.lookup vmap <$> readPretty t)
        & help   .~ choiceHelp (appName <> " versions") vss
        & defArg .~ fmap showPretty (maybeLast vss)

    let (unresolvedLibs, libsToInstall) = Repo.resolve repo appPkgDesc
    when (not $ null unresolvedLibs) . raise' $ UnresolvedDepsError unresolvedLibs

    installConfig <- get @InstallConfig
    appPath <- askOrUse (opts ^. Opts.selectedInstallationPath)
        $ question "Select installation path" plainTextReader
        & defArg .~ Just (toTextIgnore (installConfig ^. defaultBinPath)) --TODO uzyć toText i złapać tryRight'

    -- print $ "TODO: Install the app (with progress bar): "  <> appName

    let pkgPath = appPkgDesc ^. path
    home <- getHomePath
    print $ show home

    installPath <- expand $ (fromText appPath) </> (fromText appName) </> (fromText appVersion)
    createDirIfMissingTrue installPath
    print $ show installPath
    case currentHost of
        Linux   -> do
            appimage <- downloadWithProgressBar pkgPath installPath
            makeExecutable appimage
            exec <- view execName <$> get @InstallConfig
            currentAppimage <- expand $ (fromText appPath) </> (fromText exec)
            let localBinDir = home </> ".local/bin"
                localBin = home </> ".local/bin" </> (fromText exec)
            createDirIfMissingTrue localBinDir
            createSymLink appimage currentAppimage
            createSymLink currentAppimage localBin
            shell <- checkShell
            exportPath localBinDir shell
        Darwin  -> return ()
        Windows -> do
            tmp <- getTmpPath
            zippedPackage <- downloadWithProgressBar pkgPath tmp
            let scriptPath = "https://s3-us-west-2.amazonaws.com/packages-luna/windows/j_unzip.vbs" -- do osobnego statea dla windows installation utils?
            script <- downloadFromURL scriptPath

            print zippedPackage
            print script
            unpacked <- Shelly.shelly $ unzipFileWindows script zippedPackage tmp
            let packageFolder = basename zippedPackage
                unzippedPackagePath = tmp </> packageFolder
            Shelly.shelly $ Shelly.cp_r unzippedPackagePath installPath
            let services = installPath </> packageFolder </> (fromText "services") --sama względna ścieżka do serwisów też do tego samego statea windows installation utils
            Shelly.shelly $ runServicesWindows services
            return ()


    -- print $ "TODO: Install the libs (each with separate progress bar): " <> show libsToInstall -- w ogóle nie supportujemy przeciez instalowania osobnych komponentów i libów
    print $ "TODO: Add new exports to bashRC if not already present"
    print $ "TODO: IMPORTANT: be sure that installation of manager updates the manager in-place"
    -- TODO: powinnismy zrobic funckje "installPackage" i przemapowac ja przez app i libsToInstall
    --       i to powinien byc koniec "instalacji" - potem jeszcze dopisywanie do shelli sciezek etc



    return ()
