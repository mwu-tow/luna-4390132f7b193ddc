{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Luna.Manager.Archive where

import Luna.Manager.System.Path

import qualified Shelly.Lifted as Shelly
import qualified Data.Text as Text
default (Text.Text)

unpackTarGzUnix :: Shelly.MonadSh m => FilePath -> FilePath -> m ()
unpackTarGzUnix file dst = do
    Shelly.cd (Shelly.fromText dst)
    Shelly.cmd  "tar" "-xf" file " --strip 1"
