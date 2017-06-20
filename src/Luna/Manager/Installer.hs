module Luna.Manager.Installer where

import Prologue

import Luna.Manager.Config.Class
import Luna.Manager.System.Host
import Luna.Manager.System.Config
import Luna.Manager.Repository
import Luna.Manager.Version
import Luna.Manager.Network

import Control.Monad.Raise
import Control.Monad.State.Layered


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

instance {-# OVERLAPPABLE #-} Monad m => MonadDefaultConfig InstallConfig sys m where
    defaultConfig = return $ InstallConfig
        { _execName        = "luna-studio"
        , _defaultConfPath = "~/.luna"
        , _defaultBinPath  = "~/.luna-bin"
        , _localName       = "local"
        }

instance Monad m => MonadDefaultConfig InstallConfig 'MacOS m where
    defaultConfig = reconfig <$> defaultConfigFor @Linux where
        reconfig cfg = cfg & execName       .~ "LunaStudio"
                           & defaultBinPath .~ "~/Applications"

instance Monad m => MonadDefaultConfig InstallConfig 'Windows m where
    defaultConfig = reconfig <$> defaultConfigFor @Linux where
        reconfig cfg = cfg & execName       .~ "LunaStudio"
                           & defaultBinPath .~ "C:\\ProgramFiles"



-----------------------
-- === Installer === --
-----------------------

data InstallOpts = InstallOpts { _selectedComponent :: Maybe Text
                               , _selectedVersion   :: Maybe Version
                               }

type MonadInstall m = (MonadStates '[SystemConfig, InstallConfig, RepoConfig] m, MonadNetwork m)

harcodedRepo :: Repo
harcodedRepo = mempty & apps . at "studio"   .~ Just (Package $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux Arch64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])
                      & apps . at "compiler" .~ Just (Package $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux Arch64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])

runInstaller :: MonadInstall m => m ()
runInstaller = do
    conf <- getRepoConf
    -- let appsNames = Map.keys $ conf ^. apps
    -- -- pytnaie co chcesz instalowac
    -- let chosenApp = undefined :: Text
    -- -- pytanie ktora wersja
    -- let chosenVersion = undefined :: Version
    --
    --   allLunaIds = map Main.id lunaVersions
    --   lastLunaId = maximum allLunaIds
    -- absDefault       <- mkPathWithHome [defaultInstallFolderName]
    -- location         <- askQuestion (Text.pack "Where to install Luna-Studio?") (Text.pack absDefault) -- defaultBinPath
    --
    -- let address = mapM (getAddress versionToinstall) lunaVersions -- dla konkretnej wersji
    --   justAddressesList = failIfNothing "cannot read URI" address
    --   justAddress = failIfNothing "cannot read URI" $ listToMaybe justAddressesList
    -- --installation
    -- locWithVersion <- mkPathWithHome [location, (Text.pack $ show versionToinstall)]
    -- createDirectory locWithVersion
    -- setCurrentDirectory locWithVersion
    -- downloadWithProgressBar justAddress
    -- let name = fromMaybe "cannot read URI" $ takeFileName justAddress
    -- appimage <- mkRelativePath [(Text.pack locWithVersion), name]
    -- makeExecutable appimage
    -- appimageToLunaStudio <- mkRelativePath [location, studioName]
    -- binPath <- mkPathWithHome [".local/bin", studioName]
    -- createFileLink appimage appimageToLunaStudio
    -- createFileLink appimageToLunaStudio binPath
    -- checkShell
    return ()
