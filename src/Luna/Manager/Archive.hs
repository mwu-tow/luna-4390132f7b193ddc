{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Archive where

import Control.Monad.Raise

import Luna.Manager.Shell.Commands
import Luna.Manager.Network
import Luna.Manager.System.Host

import Prologue hiding (FilePath)

import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, fromText, filename, directory, extension, basename)
import qualified Shelly.Lifted as Shelly
import           Shelly.Lifted (MonadSh)
import qualified Data.Text as Text
default (Text.Text)

data ExtensionError = ExtensionError deriving (Show)
instance Exception ExtensionError

extensionError :: SomeException
extensionError = toException ExtensionError

unpackArchive :: (MonadIO m, MonadNetwork m) => FilePath -> m FilePath
unpackArchive file = case currentHost of
    Windows -> unzipFileWindows file
    Darwin  -> Shelly.shelly $ unpackTarGzUnix file
    Linux   -> do
        ext <- tryJust extensionError $ extension file
        case ext of
            "AppImage" -> return file
            "gz"       -> Shelly.shelly $ unpackTarGzUnix file


unpackTarGzUnix :: Shelly.MonadSh m => FilePath -> m FilePath
unpackTarGzUnix file = do
    let dir = directory file
        name = basename file
    Shelly.cd dir
    Shelly.cmd  "tar" "-xpzf" file "--strip=1" name
    return name

-- TODO: download unzipper if missing
unzipFileWindows :: (MonadIO m, MonadNetwork m)=> FilePath -> m FilePath
unzipFileWindows zipFile = do
    let scriptPath = "https://s3-us-west-2.amazonaws.com/packages-luna/windows/j_unzip.vbs"
    --sprawdź czy jest na dysku, shelly.find, skrypt i plik musza byc w tym samym directory

    script <- downloadFromURL scriptPath
    let dir = directory zipFile
        name = basename zipFile
    -- Shelly.shelly $ Shelly.cp script dir
    Shelly.shelly $ Shelly.cd dir
    Shelly.shelly $ Shelly.cmd "cscript" (filename script) (filename zipFile)
    return name --checkon windows if it is possible to strip and unpack to different dir
