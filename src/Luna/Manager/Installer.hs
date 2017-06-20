module Luna.Manager.Installer where

import Prologue

import Luna.Manager.Config.Class
import Luna.Manager.System.Host
import Luna.Manager.System.Config
import Luna.Manager.Repository
import Luna.Manager.Version
import Luna.Manager.Network
import Luna.Manager.Config.Aeson
import Luna.Manager.Pretty

import Control.Monad.Raise
import Control.Monad.State.Layered
import System.IO (hFlush, stdout)

import qualified Data.Map as Map


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

-- === Options === --

data InstallOpts = InstallOpts { _selectedComponent        :: Maybe Text
                               , _selectedVersion          :: Maybe Text
                               , _selectedInstallationPath :: Maybe Text
                               } deriving (Show)
makeLenses ''InstallOpts

instance Default InstallOpts where def = InstallOpts def def def


-- === Errors === --

data InvalidOptionError = InvalidOptionError deriving (Show)
instance Exception InvalidOptionError where
    displayException _ = "Invalid option provided."

invalidOptionError :: SomeException
invalidOptionError = toException InvalidOptionError


-- === Utils === --

type MonadInstall m = (MonadStates '[SystemConfig, InstallConfig, RepoConfig] m, MonadNetwork m)

hardcodedRepo :: Repo
hardcodedRepo = Repo defapps deflibs "studio" where
    deflibs = mempty
    defapps = mempty & at "studio"   .~ Just (Package "studio synopsis"   $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux Arch64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])
                     & at "compiler" .~ Just (Package "compiler synopsis" $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux Arch64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])
                     & at "manager"  .~ Just (Package "manager synopsis"  $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux Arch64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])


listItem :: Text -> Text
listItem n = "  • " <> n

listItems :: [Text] -> Text
listItems = intercalate "\n" . fmap listItem

runInstaller :: MonadInstall m => InstallOpts -> m ()
runInstaller opts = do
    let repo = hardcodedRepo
    -- repo <- getRepo -- FIXME[WD]: this should be enabled instead of line above

    let appDescs    = Map.assocs $ repo ^. apps
        help        = Just $ "Available components:\n" <> listItems (uncurry (<>) . fmap (view synopsis) <$> appDescs)
        validator t = maybe (Left $ "Unknown component '" <> t <> "' selected.") (Right . (t,)) $ Map.lookup t (repo ^. apps)
        question    = "Select component to be installed"
        defResp     = Just $ repo ^. defaultApp
    (appName, appPkg) <- askOrUse (opts ^. selectedComponent) validator help defResp question
    putStrLn . convert $ "Installing package '" <> appName <> "'"

    let appDescs    = Map.assocs $ appPkg ^. versions
        help        = Just $ "Available versions:\n" <> listItems (showPretty . fst <$> appDescs)
        validator t = maybe (Left $ "Unknown version '" <> t <> "' selected.") (Right . (t,)) $ Map.lookup (undefined) (appPkg ^. versions)
        question    = "Select version to be installed"
        defResp     = Nothing -- Just $ repo ^. defaultVersion
    (version, sysMap) <- askOrUse (opts ^. selectedVersion) validator help defResp question


    -- -- pytnaie co chcesz instalowac
    -- let chosenApp = undefined :: Text
    -- -- pytanie ktora wersja
    -- let chosenVersion = undefined :: Version
    --
    --   allLunaIds = map Main.id lunaVersions
    --   lastLunaId = maximum allLunaIds
    -- absDefault       <- mkPathWithHome [defaultInstallFolderName]
    -- location         <- ask (Text.pack "Where to install Luna-Studio?") (Text.pack absDefault) -- defaultBinPath
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

askOrUse :: (MonadIO m, MonadException SomeException m) => Maybe Text -> (Text -> Either Text a) -> Maybe Text -> Maybe Text -> Text -> m a
askOrUse mdef validator help defResp question = case mdef of
    Nothing -> ask validator help defResp question
    Just s  -> validate validator (raise invalidOptionError) s

ask :: MonadIO m => (Text -> Either Text a) -> Maybe Text -> Maybe Text -> Text -> m a
ask validator help defResp question = validate validator (ask validator help defResp question) =<< askRaw help defResp question

askRaw :: MonadIO m => Maybe Text -> Maybe Text -> Text -> m Text
askRaw help defResp question = do
    let defRespSfx   = maybe "" (\s -> " [" <> s <> "]") defResp
        questionLine = question <> defRespSfx <> ": "
        printHeader  = do
            putStrLn ""
            mapM (putStrLn . convert) help
        goQuestion   = do
            resp <- askLine
            if resp /= "" then return resp
                else maybe goQuestion return defResp
        askLine      = do
            putStr $ convert questionLine
            liftIO $ hFlush stdout
            liftIO $ convert <$> getLine
    printHeader
    goQuestion

validate :: MonadIO m => (Text -> Either Text a) -> m a -> Text -> m a
validate validator f resp = case validator resp of
    Left  e -> putStrLn ("Error: " <> convert e) >> f
    Right a -> return a

boolValidator :: (Text -> Bool) -> Text -> (Text -> Either Text Text)
boolValidator f e t = if f t then Right t else Left e
