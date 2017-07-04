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

import qualified Data.Char as Char
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import qualified Data.Text as Text


import qualified Data.Yaml as Yaml

import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, fromText, basename, hasExtension)
import Shelly.Lifted (toTextIgnore)
import qualified Shelly.Lifted as Shelly
import Luna.Manager.Archive



-- FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME FIXME
-- FIXME: Remove it as fast as we upload config yaml to the server
hardcodedRepo :: Repo
hardcodedRepo = Repo defpkgs ["studio"] where
    defpkgs = mempty & at "lib1"         .~ Just (Package "lib1 synopsis"     $ fromList [ (Version 1 0 0 Nothing      , fromList [(SysDesc Linux X64, PackageDesc mempty "path")] )])
                     & at "luna-studio"  .~ Just (Package "studio synopsis"   $ fromList [ (Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
                                                                                         , (Version 1 0 0 (Just $ RC 6), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
                                                                                         , (Version 1 1 0 Nothing      , fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 Nothing)] "path")] )
                                                                                         ])

                     & at "luna"         .~ Just (Package "compiler synopsis" $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 (Just $ RC 5))] "path")] )])
                     & at "luna-manager" .~ Just (Package "manager synopsis"  $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageHeader "lib1" (Version 1 0 0 (Just $ RC 5))] "path")] )])



---------------------------------
-- === Installation config === --
---------------------------------

-- === Definition === --

data InstallConfig = InstallConfig { _defaultConfPath :: FilePath
                                   , _defaultBinPath  :: FilePath
                                   , _localName       :: Text
                                   , _packageBinPath  :: FilePath
                                   }
makeLenses ''InstallConfig


-- === Instances === --

-- TODO: refactor out
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
    goBody = \case [] -> []
                   ('-':ss) -> goHead ss
                   (s  :ss) -> s : goBody ss

instance Monad m => MonadHostConfig InstallConfig 'Linux arch m where
    defaultHostConfig = return $ InstallConfig
        { _defaultConfPath = "~/.luna"
        , _defaultBinPath  = "~/.luna-bin"
        , _localName       = "local"
        , _packageBinPath  = "bin"
        }

instance Monad m => MonadHostConfig InstallConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & defaultBinPath .~ "/Applications"

instance Monad m => MonadHostConfig InstallConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & defaultBinPath .~ "C:\\Program Files"



-----------------------
-- === Installer === --
-----------------------

-- === Errors === --

newtype UnresolvedDepsError = UnresolvedDepsError [PackageHeader] deriving (Show)
makeLenses ''UnresolvedDepsError

instance Exception UnresolvedDepsError where
    displayException err = "Following dependencies were unable to be resolved: " <> show (showPretty <$> unwrap err)

data ExecutableNotFound = ExecutableNotFound deriving (Show)
instance Exception ExecutableNotFound where
    displayException _ = "Couldn't find executable"

executableNotFound :: SomeException
executableNotFound = toException ExecutableNotFound

-- === Running === --

type MonadInstall m = (MonadStates '[EnvConfig, InstallConfig, RepoConfig] m, MonadNetwork m)

prepareInstallPath :: MonadInstall m => FilePath -> Text -> Text -> m FilePath
prepareInstallPath appPath appName appVersion = expand $ case currentHost of
    Linux   -> appPath </> convert appName </> convert appVersion
    Windows -> appPath </> convert appName </> convert appVersion
    Darwin  -> appPath </> convert (appName <> ".app") </> "Contents" </> "Resources" </> convert appVersion

downloadAndUnpack :: MonadInstall m => URIPath -> FilePath -> m ()
downloadAndUnpack pkgPath installPath = do
    Shelly.shelly $ Shelly.mkdir_p installPath
    tmp <- getTmpPath
    print $ pkgPath
    pkg <- downloadWithProgressBar pkgPath tmp
    unpacked <- unpackArchive pkg
    Shelly.shelly $ copyDir unpacked installPath

postInstallation :: MonadInstall m => FilePath -> Text -> Text -> m()
postInstallation installPath binPath appName = do
    home <- getHomePath
    case currentHost of
        Linux -> do
            --check bin dir in install path and symlink everything inside + chmod for linux
            execList <- Shelly.shelly $  Shelly.findWhen (pure . Shelly.hasExt "AppImage") installPath --może inny sposób na przekazywanie ścieżki do executabla ??
            appimage <- tryJust executableNotFound $ listToMaybe execList
            makeExecutable appimage
            currentAppimage <- expand $ (fromText binPath) </> (fromText appName)
            let localBinDir = home </> ".local/bin" -- TODO: moe to state and use the same code for windows
                localBin = home </> ".local/bin" </> (fromText appName)
            Shelly.shelly $ Shelly.mkdir_p localBinDir
            createSymLink appimage currentAppimage
            createSymLink currentAppimage localBin
            shell <- checkShell
            exportPath localBinDir shell -- rename export because it is not export to env
        Darwin  -> do
            let resourcesBin  = installPath </> (fromText appName)
                localBinDir   = home </> ".local/bin"
                localBin      = home </> ".local/bin" </> (fromText appName)
            currentBinDir <- expand $ (fromText binPath) </> (fromText (Text.append appName ".app")) </> (fromText "Contents") </> (fromText "MacOS")
            currentBin    <- expand $ (fromText binPath) </> (fromText (Text.append appName ".app")) </> (fromText "Contents") </> (fromText "MacOS") </> (fromText appName)

            Shelly.shelly $ Shelly.mkdir_p currentBinDir
            createSymLink resourcesBin currentBin
            createSymLink currentBin localBin

        Windows -> do
            let services = installPath </> (fromText "services") --sama względna ścieżka do serwisów też do tego samego statea windows installation utils
            Shelly.shelly $ runServicesWindows services
            --copy shortcut to menu programs



runInstaller :: MonadInstall m => InstallOpts -> m ()
runInstaller opts = do
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

    installConfig <- get @InstallConfig
    binPath <- askOrUse (opts ^. Opts.selectedInstallationPath)
        $ question "Select installation path" plainTextReader
        & defArg .~ Just (toTextIgnore (installConfig ^. defaultBinPath)) --TODO uzyć toText i złapać tryRight'


    installPath <- prepareInstallPath (convert binPath) (mkSystemPkgName appName) appVersion
    downloadAndUnpack (appPkgDesc ^. path) installPath
    postInstallation installPath binPath (mkSystemPkgName appName)

    -- appsToInstall = filter ((`elem` (repo ^.apps)) . view) pkgsToInstall


    -- print $ "TODO: Install the libs (each with separate progress bar): " <> show pkgsToInstall -- w ogóle nie supportujemy przeciez instalowania osobnych komponentów i libów
    print $ "TODO: Add new exports to bashRC if not already present"
    print $ "TODO: IMPORTANT: be sure that installation of manager updates the manager in-place"
    -- TODO: powinnismy zrobic funckje "installPackage" i przemapowac ja przez app i pkgsToInstall
    --       i to powinien byc koniec "instalacji" - potem jeszcze dopisywanie do shelli sciezek etc



    return ()
