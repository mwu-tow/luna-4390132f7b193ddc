{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Archive where

import Luna.Manager.Shell.Commands
import Luna.Manager.Network

import Prologue hiding (FilePath)

import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, fromText, filename, directory)
import qualified Shelly.Lifted as Shelly
import           Shelly.Lifted (MonadSh)
import qualified Data.Text as Text
default (Text.Text)



-- TODO: wspolny upack
unpackTarGzUnix :: Shelly.MonadSh m => FilePath -> m ()
unpackTarGzUnix file = do
    let dir = directory file
    Shelly.cd dir
    Shelly.cmd  "tar" "-xvpzf" file

-- TODO: download unzipper if missing
unzipFileWindows :: (MonadIO m, MonadNetwork m)=> FilePath -> m ()
unzipFileWindows zipFile = do
    let scriptPath = "https://s3-us-west-2.amazonaws.com/packages-luna/windows/j_unzip.vbs"
    --sprawdź czy jest na dysku, shelly.find, skrypt i plik musza byc w tym samym directory

    script <- downloadFromURL scriptPath
    let dir = directory zipFile
    Shelly.shelly $ Shelly.cp script dir
    Shelly.shelly $ Shelly.cd dir
    Shelly.shelly $ Shelly.cmd "cscript" (filename script) (filename zipFile)
