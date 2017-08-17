{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Prelude hiding (FilePath)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens.Aeson
import Control.Lens
import Control.Monad.State.Layered
import Data.ByteString.Lazy  (unpack)
import Data.List.Split
import Data.Semigroup ((<>))
import Filesystem.Path
import Options.Applicative
import System.Directory (doesDirectoryExist, setCurrentDirectory, getHomeDirectory, getCurrentDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode)
import System.Process.Typed (shell, runProcess, runProcess_, setWorkingDir, setEnv, readProcess_)
import System.Environment (getExecutablePath)
import qualified System.Environment  as Environment
import qualified System.IO as IO
import Data.Maybe (fromMaybe)
import Filesystem.Path.CurrentOS (decodeString, encodeString, fromText)
import qualified Shelly.Lifted as Shelly
import System.Host

-- import           Luna.Manager.System.Host

import qualified Data.Text as T
default (T.Text)


data RunnerConfig = RunnerConfig { _versionFile            :: FilePath
                                 , _mainHomeDir            :: FilePath
                                 , _userConfigFolder       :: FilePath
                                 , _configFolder           :: FilePath
                                 , _configHomeFolder       :: FilePath
                                 , _studioHome             :: FilePath
                                 , _logsFolder             :: FilePath
                                 , _atomPackageName        :: T.Text
                                 , _appName                :: T.Text
                                 , _supervisorFolder       :: FilePath
                                 , _supervisordFolder      :: FilePath
                                 , _supervisordBin         :: FilePath
                                 , _atomFolder             :: FilePath
                                 , _thirdPartyFolder       :: FilePath
                                 , _backendBinsFolder      :: FilePath
                                 , _binsFolder             :: FilePath
                                 , _packageFolder          :: FilePath
                                 , _supervisorKillFolder   :: FilePath
                                 , _supervisorKillBin      :: FilePath
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
        , _atomFolder             = "atom"
        , _thirdPartyFolder       = "third-party"
        , _backendBinsFolder      = "private"
        , _binsFolder             = "bin"
        , _packageFolder          = "packages"
        , _supervisorKillFolder   = "kill"
        , _supervisorKillBin      = "kill"
        }

instance Monad m => MonadHostConfig RunnerConfig 'Darwin arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg

instance Monad m => MonadHostConfig RunnerConfig 'Windows arch m where
    defaultHostConfig = reconfig <$> defaultHostConfigFor @Linux where
        reconfig cfg = cfg



scriptDir :: MonadIO m => m FilePath
scriptDir = do
  localExePath <- liftIO $ getExecutablePath
  return $ directory $ decodeString $ localExePath

mainAppDir :: MonadIO m => m FilePath
mainAppDir = liftIO $ do
    scriptPath <- scriptDir
    return $ parent $ parent $ parent $ scriptPath

version :: (MonadRun m, MonadIO m) => m T.Text
version = do
    runnerCfg <- get @RunnerConfig
    main <- mainAppDir
    let versionFilePath = main </> (runnerCfg ^. configFolder) </> (runnerCfg ^. versionFile)
    version <- liftIO $ readFile $ encodeString versionFilePath
    return $ T.pack $ version

backendBinsPath :: (MonadRun m, MonadIO m) => m FilePath
backendBinsPath = do
    runnerCfg <- get @RunnerConfig
    main <- mainAppDir
    return $ main </> (runnerCfg ^. binsFolder) </> (runnerCfg ^. backendBinsFolder)

configPath :: (MonadRun m, MonadIO m) => m FilePath
configPath = do
    runnerCfg <- get @RunnerConfig
    main <- mainAppDir
    return $ main </> (runnerCfg ^. configFolder)

atomAppPath :: (MonadRun m, MonadIO m) => m FilePath
atomAppPath = do
    runnerCfg <- get @RunnerConfig
    main <- mainAppDir
    case currentHost of
        Linux   -> return $ main </> (runnerCfg ^. thirdPartyFolder) </> "atom" </> "usr" </> "bin" </> "atom"
        Darwin  -> return $ main </> (runnerCfg ^. thirdPartyFolder) </> "Atom.app" </> "Contents" </> "MacOS" </> "Atom"
        Windows -> return $ main </> (runnerCfg ^. thirdPartyFolder) </> "Atom" </> "atom.exe"

backendDir :: (MonadRun m, MonadIO m) => m FilePath
backendDir = do
    runnerCfg <- get @RunnerConfig
    main <- mainAppDir
    return $ main </> (runnerCfg ^. configFolder) </> (runnerCfg ^. supervisorFolder)

supervisordBinPath :: (MonadRun m, MonadIO m) => m FilePath
supervisordBinPath = do
    runnerCfg <- get @RunnerConfig
    main <- mainAppDir
    return $ main </> (runnerCfg ^. thirdPartyFolder) </> (runnerCfg ^. supervisordFolder) </> (runnerCfg ^. supervisordBin)

killSupervisorBinPath :: (MonadRun m, MonadIO m) => m FilePath
killSupervisorBinPath = do
    runnerCfg <- get @RunnerConfig
    main <- mainAppDir
    return $ main </> (runnerCfg ^. thirdPartyFolder) </> (runnerCfg ^. supervisorKillFolder) </> (runnerCfg ^. supervisorKillBin)

packageStudioAtomHome :: (MonadRun m, MonadIO m) => m FilePath
packageStudioAtomHome = do
    runnerCfg <- get @RunnerConfig
    main <- mainAppDir
    return $ main </> (runnerCfg ^. userConfigFolder) </> (runnerCfg ^. studioHome)

userStudioAtomHome :: (MonadRun m, MonadIO m) => m FilePath
userStudioAtomHome = do
    runnerCfg <- get @RunnerConfig
    home      <- liftIO $ getHomeDirectory
    v         <- version
    return $ decodeString home </> (runnerCfg ^. mainHomeDir) </> (runnerCfg ^. configHomeFolder) </> fromText (runnerCfg ^. appName) </> fromText v

localLogsDirectory :: (MonadRun m, MonadIO m) => m FilePath
localLogsDirectory = do
    main <- mainAppDir
    return $  main </> decodeString "logs"




userLogsDirectory :: (MonadRun m, MonadIO m) => m FilePath
userLogsDirectory = do
    runnerCfg <- get @RunnerConfig
    home      <- liftIO $ getHomeDirectory
    v         <- version
    return $ decodeString home </> (runnerCfg ^. mainHomeDir) </> (runnerCfg ^. logsFolder) </> fromText (runnerCfg ^. appName) </> fromText v

copyLunaStudio :: (MonadRun m, MonadIO m) => m ()
copyLunaStudio = do
  packageAtomHome <- packageStudioAtomHome
  atomHome <- userStudioAtomHome
  Shelly.shelly $ Shelly.mkdir_p atomHome
  Shelly.shelly $ Shelly.cp_r packageAtomHome atomHome

testDirectory :: MonadIO m => FilePath -> m Bool
testDirectory path = Shelly.shelly $ Shelly.test_d path


checkLunaHome :: (MonadRun m, MonadIO m) => m ()
checkLunaHome = do
    runnerCfg       <- get @RunnerConfig
    userAtomHome    <- userStudioAtomHome
    let pathLunaPackage = userAtomHome </> (runnerCfg ^. packageFolder) </> fromText (runnerCfg ^. atomPackageName)
    testDirectory pathLunaPackage >>= \case
        True -> return ()
        False -> copyLunaStudio

runLunaEmpireMacOS :: (MonadRun m, MonadIO m) => FilePath -> T.Text -> m ()
runLunaEmpireMacOS logs configFile = do
    lunaSupervisor <- backendDir
    supervisord    <- supervisordBinPath
    Shelly.shelly $ Shelly.mkdir_p logs
    runProcess_ $ setWorkingDir (encodeString lunaSupervisor) $ shell ((encodeString supervisord) ++ " -n -c " ++ (T.unpack configFile)) -- done with system.process.typed because with shelly clicking on app in launchpad returned abnormal exit code


runLunaEmpire :: (MonadRun m, MonadIO m) => FilePath -> T.Text -> m ()
runLunaEmpire logs configFile = do
    lunaSupervisor <- backendDir
    supervisord    <- supervisordBinPath
    Shelly.shelly $ Shelly.mkdir_p logs
    Shelly.shelly $ do
        Shelly.cd lunaSupervisor
        Shelly.cmd supervisord "-n" "-c" configFile

runFrontend :: (MonadRun m, MonadIO m) => Maybe T.Text -> m ()
runFrontend args = do
    atomHome    <- packageStudioAtomHome
    atom        <- atomAppPath
    liftIO $ Environment.setEnv "ATOM_HOME" (encodeString $ atomHome </> "atom")
    case currentHost of
        Darwin -> case args of
            Just arg -> Shelly.shelly $ Shelly.cmd atom "-w" arg
            Nothing  -> Shelly.shelly $ Shelly.cmd atom "-w"
        Linux -> case args of
            Just arg -> Shelly.shelly $ Shelly.cmd atom "-w" arg
            Nothing  -> Shelly.shelly $ Shelly.cmd atom "-w"
        Windows -> liftIO $ print "Unsupported system"

runBackend :: (MonadRun m, MonadIO m) => m ()
runBackend = do
    logs        <- localLogsDirectory
    backendBins <- backendBinsPath
    config      <- configPath
    liftIO $ Environment.setEnv "SUPERVISORLOGS" (encodeString logs)
    liftIO $ Environment.setEnv "BACKENDBINSDIR" (encodeString backendBins)
    liftIO $ Environment.setEnv "CONFIG" (encodeString config)
    case currentHost of
        Darwin -> do
            runLunaEmpireMacOS logs "supervisord.conf"
        Linux -> do
            runLunaEmpire logs "supervisord.conf"
        Windows -> liftIO $ print "Unsupported system"

runLocal :: (MonadRun m, MonadIO m) => m ()
runLocal = do
    atomHome    <- packageStudioAtomHome
    logs        <- localLogsDirectory
    backendBins <- backendBinsPath
    atom        <- atomAppPath
    config      <- configPath
    kill        <- killSupervisorBinPath
    liftIO $ Environment.setEnv "LUNAATOM" (encodeString $ atomHome </> "atom")
    liftIO $ Environment.setEnv "SUPERVISORLOGS" (encodeString logs)
    liftIO $ Environment.setEnv "BACKENDBINSDIR" (encodeString backendBins)
    liftIO $ Environment.setEnv "ATOM" (encodeString atom)
    liftIO $ Environment.setEnv "CONFIG" (encodeString config)
    liftIO $ Environment.setEnv "KILL" (encodeString kill)
    case currentHost of
        Darwin -> do
            runLunaEmpireMacOS logs "supervisord-mac.conf"
        Linux -> do
            runLunaEmpire logs "supervisord-linux.conf"

        Windows -> liftIO $ print "Unsupported system"

runPackage :: (MonadRun m, MonadIO m) => m ()
runPackage = case currentHost of
    Darwin -> do
        atomHome    <- userStudioAtomHome
        logs        <- userLogsDirectory
        backendBins <- backendBinsPath
        atom        <- atomAppPath
        config      <- configPath
        kill        <- killSupervisorBinPath
        liftIO $ Environment.setEnv "LUNAATOM" (encodeString $ atomHome </> "atom")
        liftIO $ Environment.setEnv "SUPERVISORLOGS" (encodeString logs)
        liftIO $ Environment.setEnv "BACKENDBINSDIR" (encodeString backendBins)
        liftIO $ Environment.setEnv "ATOM" (encodeString atom)
        liftIO $ Environment.setEnv "CONFIG" (encodeString config)
        liftIO $ Environment.setEnv "KILL" (encodeString kill)
        checkLunaHome
        runLunaEmpireMacOS logs "supervisord-mac.conf"
    Linux -> do
        atomHome    <- userStudioAtomHome
        logs        <- userLogsDirectory
        backendBins <- backendBinsPath
        atom        <- atomAppPath
        config      <- configPath
        kill        <- killSupervisorBinPath
        liftIO $ Environment.setEnv "LUNAATOM" (encodeString $ atomHome </> "atom")
        liftIO $ Environment.setEnv "SUPERVISORLOGS" (encodeString logs)
        liftIO $ Environment.setEnv "BACKENDBINSDIR" (encodeString backendBins)
        liftIO $ Environment.setEnv "ATOM" (encodeString atom)
        liftIO $ Environment.setEnv "CONFIG" (encodeString config)
        liftIO $ Environment.setEnv "KILL" (encodeString kill)
        checkLunaHome
        runLunaEmpire logs "supervisord-linux.conf"

    Windows -> do
        atomHome <- userStudioAtomHome
        atom <- atomAppPath
        checkLunaHome
        liftIO $ Environment.setEnv "ATOM_HOME" (encodeString $ atomHome </> "atom")
        Shelly.shelly $ Shelly.cmd atom

runApp :: (MonadRun m, MonadIO m) => Maybe String -> m ()
runApp atom = do
    v <- version
    case atom of
        Just arg -> liftIO $ Environment.setEnv "ATOM_ARG" arg
        Nothing  -> liftIO $ Environment.setEnv "ATOM_ARG" " "
    if v == "develop" then runLocal else runPackage

data Options = Options
    { frontend :: Bool
    , backend  :: Bool
    , atom     :: Maybe String} deriving Show


optionParser :: Parser Options
optionParser = Options
    <$> switch (long "frontend" <> short 'f')
    <*> switch (long "backend" <> short 'b')
    <*> (optional $ strOption $ long "atom" <> short 'a')



run :: (MonadIO m) => Options -> m ()
run (Options frontend backend atom) = evalDefHostConfigs @'[RunnerConfig] $ do

    if frontend && backend
        then runApp atom --liftIO $ print "use just one or dont use any"
        else if  frontend
            then runFrontend $ T.pack <$> atom
            else if backend
                then runBackend
                else runApp atom



main :: IO ()
main =  do
    opt <- execParser (info optionParser fullDesc)
    print opt
    run opt
