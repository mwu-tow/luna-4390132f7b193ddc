{-# LANGUAGE OverloadedStrings #-}

module Luna.Manager.System where

import Prologue hiding (FilePath,null, filter, appendFile)
import System.Directory (writable, setPermissions, getPermissions, doesPathExist, getHomeDirectory)
import System.Process.Typed
import Data.List.Split (splitOn)
import Data.ByteString.Lazy (null)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (filter)
import Data.Text.IO (appendFile)
import qualified Data.Text  as Text

import Data.Maybe (listToMaybe)
import Control.Monad.Raise
import Luna.Manager.System.Env
import Luna.Manager.System.Path (FilePath,  (</>))

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
    pathCheck <- liftIO $ doesPathExist $ convert location
    return $ if pathCheck then Just location else Nothing


--TODO wyextrachowac wspolna logike dla poszczególnych terminali
exportPath :: (MonadException Text m, MonadIO m) => FilePath -> Shell -> m ()
exportPath pathToExport shellType =
    case shellType of
        Bash    -> do
            filesAvailable <- mapM runControlCheck [".bashrc", ".bash_profile", ".profile"]
            let justFiles = catMaybes $ filesAvailable
                exportToAppend = Text.concat ["export PATH=", pathToExport, ":$PATH"]
            file <- maybe (raise ("Bash config not found" :: Text)) return (listToMaybe justFiles)
            liftIO $ appendFile (convert file) exportToAppend
        Zsh     -> do
            filesAvailable <- mapM runControlCheck [".zshrc", ".zprofile"]
            let justFiles = catMaybes $ filesAvailable
                exportToAppend = Text.concat ["path+=", pathToExport]
            file <- maybe (raise ("Bash config not found" :: Text)) return (listToMaybe justFiles)
            liftIO $ appendFile (convert file) exportToAppend
        Unknown -> raise ("Unrecognized shell. Please add ~/.local/bin to your exports." :: Text)


makeExecutable :: MonadIO m => FilePath -> m ()
makeExecutable file = liftIO $ do
    p <- getPermissions $ convert file
    setPermissions (convert file) (p {writable = True})
