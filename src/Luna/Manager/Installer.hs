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

data InstallOpts = InstallOpts { _selectedComponent :: Maybe Text
                               , _selectedVersion   :: Maybe Version
                               } deriving (Show)
makeLenses ''InstallOpts

instance Default InstallOpts where def = InstallOpts def def


-- === Errors === --

data InstallError = InstallError deriving (Show)
instance Exception InstallError

installError :: SomeException
installError = toException InstallError


-- === Utils === --

type MonadInstall m = (MonadStates '[SystemConfig, InstallConfig, RepoConfig] m, MonadNetwork m)

hardcodedRepo :: Repo
hardcodedRepo = Repo defapps deflibs "studio" where
    deflibs = mempty
    defapps = mempty & at "studio"   .~ Just (Package "studio synopsis"   $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux Arch64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])
                     & at "compiler" .~ Just (Package "compiler synopsis" $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux Arch64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])
                     & at "manager"  .~ Just (Package "manager synopsis"  $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux Arch64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])


componentDesc :: Text -> Text -> Text
componentDesc n s = "  • " <> n <> " " <> s

runInstaller :: MonadInstall m => InstallOpts -> m ()
runInstaller opts = do
    let repo = hardcodedRepo
    -- repo <- getRepo -- FIXME[WD]: this should work instead of line above

    let appDescs    = Map.assocs $ repo ^. apps
        appNames    = fst <$> appDescs
        help        = Just $ "Available components:\n" <> intercalate "\n" (uncurry componentDesc . fmap (view synopsis) <$> appDescs)
        validator s = boolValidator (`elem` appNames) ("Unknown component '" <> s <> "' selected.") s
        question    = "Select component to be installed"
        defResp     = Just $ repo ^. defaultApp
    askOrUse (opts ^. selectedComponent) validator help defResp question
    print appNames
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

askOrUse :: (MonadIO m, ValueReader a, MonadException SomeException m) => Maybe a -> (a -> Either Text a) -> Maybe Text -> Maybe Text -> Text -> m a
askOrUse mdef v help defResp question = case mdef of
    Nothing -> ask v help defResp question
    Just s  -> validate v (raise installError) s

ask :: (MonadIO m, ValueReader a) => (a -> Either Text a) -> Maybe Text -> Maybe Text -> Text -> m a
ask v help defResp question = readAndValidate v (ask v help defResp question) =<< askRaw help defResp question

askRaw :: MonadIO m => Maybe Text -> Maybe Text -> Text -> m Text
askRaw help defResp question = do
    let defRespSfx   = maybe "" (\s -> " [" <> s <> "]") defResp
        questionLine = question <> defRespSfx <> ": "
    putStrLn ""
    mapM (putStrLn . convert) help
    putStr $ convert questionLine
    liftIO $ hFlush stdout
    resp <- liftIO $ convert <$> getLine
    return $ maybe resp (\a -> if resp == "" then a else resp) defResp

readAndValidate :: (MonadIO m, ValueReader a) => (a -> Either Text a) -> m a -> Text -> m a
readAndValidate v f resp = case readValue resp of
    Left  e -> putStrLn ("Error: " <> convert e) >> f
    Right a -> validate v f a

validate :: (MonadIO m, ValueReader a) => (a -> Either Text a) -> m a -> a -> m a
validate v f resp = case v resp of
        Right t -> return t
        Left  e -> putStrLn ("Error: " <> convert e) >> f


boolValidator :: (Text -> Bool) -> Text -> (Text -> Either Text Text)
boolValidator f e t = if f t then Right t else Left e


class    ValueReader a    where readValue :: Text -> Either Text a
instance ValueReader Text where readValue = Right
