{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Prelude                       hiding (FilePath)
import           Control.Lens.Aeson
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.State.Layered
import           Data.ByteString.Lazy          (unpack)
import           Data.List.Split
import qualified Data.List                     as List
import           Data.Maybe                    (fromMaybe, maybeToList)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS     (decodeString, encodeString, fromText)
import           Options.Applicative
import           System.Directory              (doesDirectoryExist, setCurrentDirectory, getHomeDirectory, getCurrentDirectory, createDirectoryIfMissing)
import           System.Exit                   (ExitCode)
import           System.Process.Typed          (shell, runProcess, runProcess_, setWorkingDir, readProcess_)
import           System.Environment            (getExecutablePath, getArgs)
import qualified System.Environment            as Environment
import qualified System.IO                     as IO
import qualified Shelly.Lifted                 as Shelly
import           System.Host

default (T.Text)


data RunnerConfig = RunnerConfig { _versionFile            :: FilePath
                                 , _mainHomeDir            :: FilePath
                                 , _userConfigFolder       :: FilePath
                                 , _configFolder           :: FilePath
                                 , _configHomeFolder       :: FilePath
                                 , _studioHome             :: FilePath
                                 , _logsFolder             :: FilePath
                                 , _atomPackageName        :: FilePath
                                 , _appName                :: FilePath
                                 , _supervisorFolder       :: FilePath
                                 , _supervisordFolder      :: FilePath
                                 , _supervisordBin         :: FilePath
                                 , _supervisordConfig      :: FilePath
                                 , _atomFolder             :: FilePath
                                 , _thirdPartyFolder       :: FilePath
                                 , _backendBinsFolder      :: FilePath
                                 , _binsFolder             :: FilePath
                                 , _packageFolder          :: FilePath
                                 , _supervisorKillFolder   :: FilePath
                                 , _supervisorKillBin      :: FilePath
                                 , _atomBinPath            :: FilePath
                                 }

makeLenses ''RunnerConfig

type MonadRun m = (MonadStates '[RunnerConfig] m, MonadIO m)

instance Monad m => MonadHostConfig RunnerConfig 'Linux arch m where
    defaultHostConfig = return $ RunnerConfig
        { _versionFile            = "version.txt"
        , _mainHomeDir            = ".luna"
        , _userConfigFolder       = "user-config"
        , _configFolder           = "config"
        , _configHomeFolder       = "config"
        , _studioHome             = "atom"
        , _logsFolder             = "logs"
        , _atomPackageName        = "luna-studio"
        , _appName                = "luna-studio"
        , _supervisorFolder       = "supervisor"
        , _supervisordFolder      = "supervisord"
        , _supervisordBin         = "supervisord"
        , _supervisordConfig      = "supervisord-package.conf"
        , _atomFolder             = "atom"
        , _thirdPartyFolder       = "third-party"
        , _backendBinsFolder      = "private"
        , _binsFolder             = "bin"
        , _packageFolder          = "packages"
        , _supervisorKillFolder   = "kill"
        , _supervisorKillBin      = "kill"
        , _atomBinPath            = "atom" </> "usr" </> "bin" </> "atom"
        }

instance Monad m => MonadHostConfig RunnerConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & atomBinPath .~ ("Atom.app" </> "Contents" </> "MacOS" </> "Atom")

instance Monad m => MonadHostConfig RunnerConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg & atomBinPath .~ ("Atom" </> "atom.exe")

-- path helpers --
scriptDir, mainAppDir :: MonadIO m => m FilePath
scriptDir  = (directory . decodeString) <$> liftIO getExecutablePath
mainAppDir = (parent . parent . parent) <$> scriptDir

relativeToDir :: MonadRun m => m FilePath -> [Getting FilePath RunnerConfig FilePath] -> m FilePath
relativeToDir basePath segmentAccessors = do
    runnerCfg <- get @RunnerConfig
    base      <- basePath
    let pathSegments = map (runnerCfg ^.) segmentAccessors
    return $ foldl (</>) base pathSegments

relativeToMainDir, relativeToHomeDir :: MonadRun m => [Getting FilePath RunnerConfig FilePath] -> m FilePath
relativeToMainDir = relativeToDir mainAppDir
relativeToHomeDir = relativeToDir (decodeString <$> (liftIO getHomeDirectory)) . (mainHomeDir :)

version :: MonadRun m => m FilePath
version = do
    versionFilePath <- relativeToMainDir [configFolder, versionFile]
    versionStr      <- liftIO $ readFile $ encodeString versionFilePath
    return . fromText . T.pack $ versionStr

-- paths --
backendBinsPath, configPath, atomAppPath, backendDir             :: MonadRun m => m FilePath
supervisordBinPath, killSupervisorBinPath, packageStudioAtomHome :: MonadRun m => m FilePath
userStudioAtomHome, localLogsDirectory, userLogsDirectory        :: MonadRun m => m FilePath

backendBinsPath       = relativeToMainDir [binsFolder, backendBinsFolder]
configPath            = relativeToMainDir [configFolder]
atomAppPath           = relativeToMainDir [thirdPartyFolder, atomBinPath]
backendDir            = relativeToMainDir [configFolder, supervisorFolder]
supervisordBinPath    = relativeToMainDir [thirdPartyFolder, supervisordFolder, supervisordBin]
killSupervisorBinPath = relativeToMainDir [thirdPartyFolder, supervisorKillFolder, supervisorKillBin]
packageStudioAtomHome = relativeToMainDir [userConfigFolder, studioHome]
localLogsDirectory    = relativeToMainDir [logsFolder]
userLogsDirectory     = relativeToHomeDir [logsFolder, appName] >>= (\p -> (fmap (p </>) version))
userStudioAtomHome = do
    runnerCfg <- get @RunnerConfig
    baseDir   <- relativeToHomeDir [configHomeFolder, appName] >>= (\p -> (fmap (p </>) version))
    return $ baseDir </> (runnerCfg ^. studioHome)

atomHomeDir, logsDir :: MonadRun m => Bool -> m FilePath
atomHomeDir develop = if develop then packageStudioAtomHome else userStudioAtomHome
logsDir     develop = if develop then localLogsDirectory    else userLogsDirectory

-- misc runner utils --

windows, linux, darwin, unix :: Bool
windows = currentHost == Windows
linux   = currentHost == Linux
darwin  = currentHost == Darwin
unix    = not windows

unixOnly :: MonadRun m => m () -> m ()
unixOnly act = case currentHost of
    Windows -> liftIO $ putStrLn "Current host (Windows) not supported for this operation"
    _       -> act

setEnv :: MonadRun m => String -> FilePath -> m ()
setEnv name path = liftIO $ Environment.setEnv name $ encodeString path

copyLunaStudio :: MonadRun m => m ()
copyLunaStudio = do
    packageAtomHome <- packageStudioAtomHome
    atomHomeParent  <- parent <$> userStudioAtomHome
    Shelly.shelly $ do
        Shelly.mkdir_p atomHomeParent
        Shelly.cp_r packageAtomHome atomHomeParent

testDirectory :: MonadIO m => FilePath -> m Bool
testDirectory path = Shelly.shelly $ Shelly.test_d path

checkLunaHome :: MonadRun m => m ()
checkLunaHome = do
    runnerCfg    <- get @RunnerConfig
    userAtomHome <- userStudioAtomHome
    let pathLunaPackage = userAtomHome </> (runnerCfg ^. packageFolder) </> (runnerCfg ^. atomPackageName)
    testDirectory pathLunaPackage >>= (\exists -> unless exists copyLunaStudio)

runLunaEmpire :: MonadRun m => FilePath -> FilePath -> m ()
runLunaEmpire logs configFile = do
    lunaSupervisor <- backendDir
    supervisord    <- supervisordBinPath
    Shelly.shelly $ Shelly.mkdir_p logs
    when darwin $
        -- done with system.process.typed because with shelly clicking on app in launchpad returned abnormal exit code
        runProcess_ $ setWorkingDir (encodeString lunaSupervisor)
                    $ shell ((encodeString supervisord) ++ " -n -c " ++ (encodeString configFile))
    when linux $ Shelly.shelly
               $ Shelly.chdir lunaSupervisor $ Shelly.cmd supervisord "-n" "-c" configFile

runFrontend :: MonadRun m => Maybe T.Text -> m ()
runFrontend args = do
    atom <- atomAppPath
    liftIO $ Environment.setEnv "LUNA_STUDIO_DEVELOP" "True"
    setEnv "ATOM_HOME" =<< packageStudioAtomHome
    unixOnly $ Shelly.shelly $ Shelly.run_ atom $ "-w" : maybeToList args

runBackend :: MonadRun m => m ()
runBackend = do
    logs <- localLogsDirectory
    setEnv "LUNA_STUDIO_LOG_PATH"     =<< localLogsDirectory
    setEnv "LUNA_STUDIO_BACKEND_PATH" =<< backendBinsPath
    setEnv "LUNA_STUDIO_CONFIG_PATH"  =<< configPath
    unixOnly $ runLunaEmpire logs "supervisord.conf"

runPackage :: MonadRun m => Bool -> m ()
runPackage develop = case currentHost of
    Windows -> do
        atom <- atomAppPath
        checkLunaHome
        setEnv "ATOM_HOME" =<< userStudioAtomHome
        Shelly.shelly $ Shelly.cmd atom

    _ -> do
        runnerCfg <- get @RunnerConfig
        logs      <- logsDir develop
        let supervisorConf = runnerCfg ^. supervisordConfig
        setEnv "LUNA_STUDIO_GUI_CONFIG_PATH" =<< atomHomeDir develop
        setEnv "LUNA_STUDIO_LOG_PATH"        =<< logsDir     develop
        setEnv "LUNA_STUDIO_BACKEND_PATH"    =<< backendBinsPath
        setEnv "LUNA_STUDIO_GUI_PATH"        =<< atomAppPath
        setEnv "LUNA_STUDIO_CONFIG_PATH"     =<< configPath
        setEnv "LUNA_STUDIO_KILL_PATH"       =<< killSupervisorBinPath
        when develop   $ liftIO $ Environment.setEnv "LUNA_STUDIO_DEVELOP" "True"
        unless develop $ checkLunaHome
        runLunaEmpire logs supervisorConf

runApp :: MonadRun m => Bool -> Maybe String -> m ()
runApp develop atom = do
    liftIO $ Environment.setEnv "LUNA_STUDIO_ATOM_ARG" (fromMaybe " " atom)
    runPackage develop

data Options = Options
    { frontend :: Bool
    , backend  :: Bool
    , develop  :: Bool
    , atom     :: Maybe String} deriving Show


optionParser :: Parser Options
optionParser = Options
    <$> switch (long "frontend" <> short 'f')
    <*> switch (long "backend" <> short 'b')
    <*> switch (long "develop" <> short 'd')
    <*> (optional $ strOption $ long "atom" <> short 'a')



run :: MonadIO m => Options -> m ()
run (Options frontend backend develop atom) = evalDefHostConfigs @'[RunnerConfig] $
    if  frontend
    then runFrontend $ T.pack <$> atom
    else if backend
    then runBackend
    else runApp develop atom

filterArg :: String -> Bool
filterArg = not . List.isInfixOf "-psn"

filterArgs :: [String] -> [String]
filterArgs = filter filterArg

filteredParser :: ParserPrefs -> ParserInfo a -> IO a
filteredParser pprefs pinfo = execParserPure pprefs pinfo . filterArgs <$> getArgs >>= handleParseResult

parser :: MonadIO m => m Options
parser = liftIO $ filteredParser p opts
    where
        opts = info (optionParser <**> helper) idm
        p    = prefs showHelpOnEmpty

main :: IO ()
main = run =<< parser
