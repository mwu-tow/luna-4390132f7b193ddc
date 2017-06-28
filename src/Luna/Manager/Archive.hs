{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Archive where

import Luna.Manager.Shell.Commands

import Prologue hiding (FilePath)

import Filesystem.Path.CurrentOS (FilePath, (</>), encodeString, toText, fromText, filename)
import qualified Shelly.Lifted as Shelly
import           Shelly.Lifted (MonadSh)
import qualified Data.Text as Text
default (Text.Text)

unpackTarGzUnix :: Shelly.MonadSh m => FilePath -> FilePath -> m ()
unpackTarGzUnix file dst = do
    Shelly.cd dst
    Shelly.cmd  "tar" "-xf" file "--strip=1"

unzipFileWindows :: Shelly.MonadSh m => FilePath -> FilePath -> FilePath -> m ()
unzipFileWindows script zipFile dst = do
  Shelly.cd dst
  Shelly.cmd "cscript" (filename script) (filename zipFile)
