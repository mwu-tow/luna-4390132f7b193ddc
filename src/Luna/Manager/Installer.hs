module Luna.Manager.Installer where

import Prologue hiding (txt)

import Luna.Manager.System.Host
import Luna.Manager.System.Env
import Luna.Manager.Repository
import Luna.Manager.Version
import Luna.Manager.Network
import Luna.Manager.Pretty
import           Luna.Manager.Cmd (InstallOpts)
import qualified Luna.Manager.Cmd as Opts

import Control.Lens.Aeson
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

instance {-# OVERLAPPABLE #-} Monad m => MonadHostConfig InstallConfig sys arch m where
    defaultHostConfig = return $ InstallConfig
        { _execName        = "luna-studio"
        , _defaultConfPath = "~/.luna"
        , _defaultBinPath  = "~/.luna-bin"
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

-- === Options === --




-- === Utils === --

type MonadInstall m = (MonadStates '[EnvConfig, InstallConfig, RepoConfig] m, MonadNetwork m)

hardcodedRepo :: Repo
hardcodedRepo = Repo defapps deflibs "studio" where
    deflibs = mempty
    defapps = mempty & at "studio"   .~ Just (Package "studio synopsis"   $ fromList [ (Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )
                                                                                     , (Version 1 0 0 (Just $ RC 6), fromList [(SysDesc Linux X64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )
                                                                                     , (Version 1 1 0 Nothing      , fromList [(SysDesc Linux X64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )
                                                                                     ])

                     & at "compiler" .~ Just (Package "compiler synopsis" $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])
                     & at "manager"  .~ Just (Package "manager synopsis"  $ fromList [(Version 1 0 0 (Just $ RC 5), fromList [(SysDesc Linux X64, PackageDesc [PackageDep "bar" (Version 1 0 0 (Just $ RC 5))] $ "foo")] )])


listItem :: Text -> Text
listItem n = "  • " <> n

listItems :: [Text] -> Text
listItems = intercalate "\n" . fmap listItem



-----------------------
-- === Questions === --
-----------------------

-- === Definition === --

type ArgReader a = Text -> Either Text a
data Question  a = Question { _txt    :: Text
                            , _reader :: ArgReader a
                            , _help   :: Maybe Text
                            , _defArg :: Maybe Text
                            }

makeLenses ''Question


-- === Errors === --

data InvalidArgError = InvalidArgError deriving (Show)
instance Exception InvalidArgError where
    displayException _ = "Invalid argument provided."

invalidArgError :: SomeException
invalidArgError = toException InvalidArgError


-- === Utils === --

question :: Text -> ArgReader a -> Question a
question t r = Question t r mempty mempty

choiceValidator  :: Text -> Text -> Maybe (Either Text a) -> Either Text a
choiceValidator' :: Text -> Text -> Maybe a               -> Either Text a
choiceValidator  s t = maybe (Left $ "Unknown " <> s <> " '" <> t <> "' selected.") id
choiceValidator' s t = choiceValidator s t . fmap Right

choiceHelp :: Pretty a => Text -> [a] -> Maybe Text
choiceHelp s ts = Just $ "Available " <> s <> ":\n" <> listItems (showPretty <$> ts)


-- === Running === --

askOrUse :: (MonadIO m, MonadException SomeException m) => Maybe Text -> Question a -> m a
askOrUse mdef q = case mdef of
    Nothing -> ask q
    Just s  -> validate (q ^. reader) (raise invalidArgError) s

ask :: MonadIO m => Question a -> m a
ask q  = validate (q ^. reader) (ask q) =<< askRaw q

askRaw :: MonadIO m => Question a -> m Text
askRaw q = do
    let defAns       = maybe "" (\s -> " [" <> s <> "]") (q ^. defArg)
        questionLine = q ^. txt <> defAns <> ": "
        printHeader  = do
            putStrLn ""
            mapM (putStrLn . convert) (q ^. help)
        goQuestion   = do
            resp <- askLine
            if resp /= "" then return resp
                else maybe goQuestion return (q ^. defArg)
        askLine      = do
            putStr $ convert questionLine
            liftIO $ hFlush stdout
            liftIO $ convert <$> getLine
    printHeader
    goQuestion

validate :: MonadIO m => ArgReader a -> m a -> Text -> m a
validate reader f resp = case reader resp of
    Left  e -> putStrLn ("Error: " <> convert e) >> f
    Right a -> return a




runInstaller :: MonadInstall m => InstallOpts -> m ()
runInstaller opts = do
    let repo = hardcodedRepo
    -- repo <- getRepo -- FIXME[WD]: this should be enabled instead of line above

    (appName, appPkg) <- askOrUse (opts ^. Opts.selectedComponent)
        $ question "Select component to be installed" (\t -> choiceValidator' "component" t $ (t,) <$> Map.lookup t (repo ^. apps))
        & help   .~ choiceHelp "components" (Map.keys $ repo ^. apps)
        & defArg .~ Just (repo ^. defaultApp)

    let vmap = Map.mapMaybe (Map.lookup currentSysDesc) $ appPkg ^. versions
        vss  = sort . Map.keys $ vmap
    (appVersion, appPkgDesc) <- askOrUse (opts ^. Opts.selectedComponent)
        $ question "Select version to be installed" (\t -> choiceValidator "version" t . sequence $ fmap (t,) . flip Map.lookup vmap <$> readPretty t)
        & help   .~ choiceHelp (appName <> " versions") vss
        & defArg .~ fmap showPretty (maybeLast vss)



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
