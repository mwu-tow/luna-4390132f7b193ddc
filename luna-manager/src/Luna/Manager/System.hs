{-# LANGUAGE OverloadedStrings #-}

module Luna.Manager.System where

import Prologue hiding (FilePath,null, filter, appendFile, toText, fromText)
import System.Directory (executable, setPermissions, getPermissions, doesPathExist, getHomeDirectory)
import System.Exit
import System.Process.Typed
import qualified System.Environment  as Environment
import Data.List.Split (splitOn)
import Data.ByteString.Lazy (ByteString, null)
import Data.ByteString.Lazy.Char8 (filter)
import Data.Text.IO (appendFile)
import qualified Data.Text  as Text
import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, parent)

import Data.Maybe (listToMaybe)
import Control.Monad.Raise
import Luna.Manager.System.Env
import Luna.Manager.System.Host

import qualified Luna.Manager.Shell.Shelly as Shelly
import Luna.Manager.Shell.Shelly (MonadSh)

import qualified System.Process.Typed as Process

data Shell = Bash | Zsh | Unknown deriving (Show)

isEnter :: Char -> Bool
isEnter a = a /= '\n'

filterEnters :: ByteString ->   ByteString
filterEnters bytestr = res where
    res = filter isEnter bytestr

callShell :: MonadIO m => Text -> m ByteString
callShell cmd = do
    (exitCode, out, err) <- liftIO $ readProcess (shell $ convert cmd)
    return $ filterEnters out

checkIfNotEmpty :: MonadIO m => Text -> m Bool
checkIfNotEmpty cmd = do
    shell <- callShell cmd
    return $ not $ null shell

checkBash :: MonadIO m => m  (Maybe Shell)
checkBash = do
    sys <- checkIfNotEmpty "$SHELL -c 'echo $BASH_VERSION'"
    if sys then return $ Just Bash else return Nothing

checkZsh :: MonadIO m => m  (Maybe Shell)
checkZsh = do
    sys <- checkIfNotEmpty "$SHELL -c 'echo $ZSH_VERSION'"
    if sys then return $ Just Zsh else return Nothing

checkShell :: MonadIO m => m Shell
checkShell = do
    bash <- checkBash
    zsh  <- checkZsh
    return $ fromMaybe Unknown $ bash <|> zsh

runControlCheck :: MonadIO m => FilePath -> m (Maybe FilePath)
runControlCheck file = do
    home <- getHomePath
    let location = home </> file
    pathCheck <- liftIO $ doesPathExist $ encodeString location
    return $ if pathCheck then Just location else Nothing

data BashConfigNotFoundError = BashConfigNotFoundError deriving (Show)
instance Exception BashConfigNotFoundError where
    displayException _ = "Bash config not found"

bashConfigNotFoundError :: SomeException
bashConfigNotFoundError = toException BashConfigNotFoundError

data UnrecognizedShellError = UnrecognizedShellError deriving (Show)
instance Exception UnrecognizedShellError where
    displayException _ = "Unrecognized shell. Please add ~/.local/bin to your exports."

unrecognizedShellError :: SomeException
unrecognizedShellError = toException UnrecognizedShellError

exportPath' :: MonadIO m => FilePath -> m ()
exportPath' pathToExport = case currentHost of
    Linux -> do
        shellType <- checkShell
        flip handleAll (exportPath (parent pathToExport) shellType) $ \_ -> print "Unrecognized shell. Please add ~/.local/bin to your exports." --"error occured, TODO[WD]"
    Darwin -> do
        shellType <- checkShell
        flip handleAll (exportPath (parent pathToExport) shellType) $ \_ -> print "Unrecognized shell. Please add ~/.local/bin to your exports."--"error occured, TODO[WD]"
    Windows -> exportPathWindows pathToExport
    -- | e == unrecognizedShellError  -> error "d"
    -- | e == bashConfigNotFoundError -> error "x"
    -- UnrecognizedShellError ->
    -- `catches` [Handler (\ (ex :: UnrecognizedShellError) -> liftIO $ putStrLn $ displayException ex),
    --                                                                               Handler (\ (ex :: BashConfigNotFoundError) -> liftIO $ putStrLn $ displayException ex)]

--TODO wyextrachowac wspolna logike dla poszczególnych terminali
exportPath :: (MonadException SomeException m, MonadIO m) => FilePath -> Shell -> m ()
exportPath pathToExport shellType = do
    let pathToExportText = convert $ encodeString pathToExport --tryRight' <<= toText pathToExport
    case shellType of
        Bash    -> do
            filesAvailable <- mapM runControlCheck [".bashrc", ".bash_profile", ".profile"]
            let justFiles = catMaybes filesAvailable
                exportToAppend = Text.concat ["export PATH=", pathToExportText, ":$PATH"]
            file <- maybe (raise' bashConfigNotFoundError) return (listToMaybe justFiles)
            liftIO $ appendFile (encodeString file) exportToAppend
        Zsh     -> do
            filesAvailable <- mapM runControlCheck [".zshrc", ".zprofile"]
            let justFiles = catMaybes filesAvailable
                exportToAppend = Text.concat ["path+=", pathToExportText]
            file <- maybe (raise' bashConfigNotFoundError) return (listToMaybe justFiles)
            liftIO $ appendFile (encodeString file) exportToAppend
        Unknown -> raise' unrecognizedShellError

exportPathWindows :: MonadIO m => FilePath -> m ()
exportPathWindows path = liftIO $ do
    (exitCode, out, err) <- Process.readProcess $ Process.shell $ "setx path \"%Path%;" ++ (encodeString $ parent path) ++ "\""
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure a -> print $ "Path was not exported." <> err  -- TODO this should be warning not print but installation was succesfull just path was not exported

makeExecutable :: MonadIO m => FilePath -> m ()
makeExecutable file = case currentHost of
    Linux -> liftIO $ do
        p <- getPermissions $ encodeString file
        setPermissions (encodeString file) (p {executable = True})
    Darwin -> liftIO $ do
        p <- getPermissions $ encodeString file
        setPermissions (encodeString file) (p {executable = True})
    Windows -> return ()


runServicesWindows :: (MonadSh m, MonadIO m, Shelly.MonadShControl m) => FilePath -> FilePath -> m ()
runServicesWindows path logsPath = Shelly.chdir path $ do
    Shelly.mkdir_p logsPath
    let installPath = path </> Shelly.fromText "installAll.bat"
    Shelly.setenv "LOGSDIR" $ Shelly.toTextIgnore logsPath
    Shelly.silently $ Shelly.cmd installPath

stopServicesWindows :: MonadIO m => FilePath -> m ()
stopServicesWindows path = Shelly.shelly $ do
    Shelly.chdir path $ do
        let uninstallPath = path </> Shelly.fromText "uninstallAll.bat"
        Shelly.silently $ Shelly.cmd uninstallPath `catch` handler where

            handler :: MonadSh m => SomeException -> m ()
            handler ex = return () -- Shelly.liftSh $ print ex
