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
import Luna.Manager.System (makeExecutable, exportPath', checkShell, runServicesWindows)

import Control.Lens.Aeson
import Control.Monad.Raise
import Control.Monad.State.Layered

import qualified Data.Char as Char
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import qualified Data.Text as Text


import qualified Data.Yaml as Yaml

import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, fromText, basename, hasExtension, parent)
import Shelly.Lifted (toTextIgnore)
import qualified Shelly.Lifted as Shelly
import System.IO (hFlush, stdout)
import Luna.Manager.Archive



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
                                   , _selectedBinPath        :: FilePath
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
        { _defaultConfPath         = "~/.luna"
        , _defaultBinPathBatchApp  = "~/.luna/bin"
        , _defaultBinPathGuiApp    = "~/.luna/bin"
        , _localName               = "local"
        , _selectedBinPath         = "current"
        }

instance Monad m => MonadHostConfig InstallConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & defaultBinPathGuiApp   .~ "/Applications"

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

type MonadInstall m = (MonadStates '[EnvConfig, InstallConfig, RepoConfig] m, MonadNetwork m)

-- === Utils === --

prepareInstallPath :: MonadInstall m => AppType -> FilePath -> Text -> Text -> m FilePath
prepareInstallPath appType appPath appName appVersion = expand $ case currentHost of
    Linux   -> appPath </> convert (mkSystemPkgName appName) </> convert appVersion
    Windows -> case appType of
        GuiApp -> appPath </> convert (mkSystemPkgName appName) </> convert appVersion
        BatchApp -> appPath </> convert appName </> convert appVersion
    Darwin  -> case appType of
        GuiApp   -> appPath </> convert ((mkSystemPkgName appName) <> ".app") </> "Contents" </> "Resources" </> convert appVersion
        BatchApp -> appPath </> convert (mkSystemPkgName appName) </> convert appVersion


downloadAndUnpack :: MonadInstall m => URIPath -> FilePath -> m ()
downloadAndUnpack pkgPath installPath = do
    Shelly.shelly $ Shelly.mkdir_p installPath
    tmp <- getTmpPath
    pkg <- downloadWithProgressBar pkgPath tmp
    putStrLn "Unpacking archive"
    unpacked <- unpackArchive pkg
    liftIO $ hFlush stdout
    putStrLn "Copying files"
    Shelly.shelly $ copyDir unpacked installPath
    liftIO $ hFlush stdout

postInstallation :: MonadInstall m => AppType -> FilePath -> Text -> Text -> m()
postInstallation appType installPath binPath appName = do
    installConfig <- get @InstallConfig
    packageBin <- case currentHost of
        Linux   -> return $ installPath </> fromText (appName <> ".AppImage")
        Darwin  -> return $ installPath </> fromText (mkSystemPkgName appName)
        Windows -> return $ installPath </> fromText ((mkSystemPkgName appName) <> ".exe")
    currentBin <- case currentHost of
        Linux -> expand $ fromText binPath </> (installConfig ^. selectedBinPath)  </> fromText (mkSystemPkgName appName)
        Darwin -> case appType of
            --TODO[1.1] lets think about making it in config
            GuiApp   -> expand $ fromText binPath </> fromText ((mkSystemPkgName appName) <> ".app") </> "Contents" </> "MacOS" </> fromText (mkSystemPkgName appName)
            BatchApp -> expand $ fromText binPath </> (installConfig ^. selectedBinPath) </> fromText (mkSystemPkgName appName)
        Windows -> case appType of
            BatchApp -> expand $ fromText binPath </> (installConfig ^. selectedBinPath) </> fromText (appName <> ".exe")
            GuiApp   -> expand $ fromText binPath </> fromText (mkSystemPkgName appName) </> (installConfig ^. selectedBinPath) </> fromText (appName <> ".exe")
    makeExecutable packageBin
    linking packageBin currentBin
    linkingLocalBin currentBin appName
    runServices installPath appType

linking :: MonadInstall m => FilePath -> FilePath -> m ()
linking src dst = do
    Shelly.shelly $ Shelly.mkdir_p $ parent dst
    createSymLink src dst


linkingLocalBin :: (MonadInstall m, MonadIO m) => FilePath -> Text -> m ()
linkingLocalBin currentBin appName = do
    home <- getHomePath
    case currentHost of
        Linux -> do
            let localBin = home </> ".local/bin" </> fromText (mkSystemPkgName appName)
            linking currentBin localBin
            exportPath' localBin
        Darwin -> do
            let localBin = "/usr/local/bin" </> fromText appName
            linking currentBin localBin
            exportPath' localBin
        Windows -> exportPath' currentBin

runServices :: MonadInstall m => FilePath -> AppType -> m ()
runServices installPath appType = case currentHost of
    Windows -> case appType of
        GuiApp -> do
            let services = installPath </> fromText "services"
            Shelly.shelly $ runServicesWindows services
        BatchApp -> return ()
    otherwise -> return ()

askLocation :: MonadInstall m => InstallOpts -> AppType -> Text -> m Text
askLocation opts appType appName = do
    installConfig <- get @InstallConfig
    let pkgInstallDefPath = case appType of
            GuiApp -> installConfig ^. defaultBinPathGuiApp
            BatchApp -> installConfig ^. defaultBinPathBatchApp
    binPath <- askOrUse (opts ^. Opts.selectedInstallationPath)
        $ question ("Select installation path for " <> appName) plainTextReader
        & defArg .~ Just (toTextIgnore pkgInstallDefPath) --TODO uzyć toText i złapać tryRight'
    return binPath

installApp :: MonadInstall m => InstallOpts -> ResolvedPackage -> m ()
installApp opts package = do
    binPath     <- askLocation opts (package ^. resolvedAppType) (package ^. header . name)
    installPath <- prepareInstallPath (package ^. resolvedAppType) (convert binPath)  (package ^. header . name) $ showPretty (package ^. header . version)
    downloadAndUnpack (package ^. desc . path) installPath
    postInstallation (package ^. resolvedAppType) installPath binPath  (package ^. header . name)

-- === Running === --

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

    let appsToInstall = filter (( <$> (^. header . name)) (`elem` (repo ^.apps))) pkgsToInstall

    binPath <- askLocation opts (appPkg ^. appType) appName -- add main app to list of applications to install
    mapM_ (installApp opts) appsToInstall
    installPath <- prepareInstallPath (appPkg ^. appType) (convert binPath) appName appVersion
    downloadAndUnpack (appPkgDesc ^. path) installPath
    postInstallation (appPkg ^. appType) installPath binPath  appName



    -- print $ "TODO: Install the libs (each with separate progress bar): " <> show pkgsToInstall -- w ogóle nie supportujemy przeciez instalowania osobnych komponentów i libów
    print $ "TODO: Add new exports to bashRC if not already present"
    print $ "TODO: IMPORTANT: be sure that installation of manager updates the manager in-place"
    -- TODO: powinnismy zrobic funckje "installPackage" i przemapowac ja przez app i pkgsToInstall
    --       i to powinien byc koniec "instalacji" - potem jeszcze dopisywanie do shelli sciezek etc



    return ()
