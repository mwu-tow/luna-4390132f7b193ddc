module Main where

import Prologue hiding (FilePath)

import Control.Monad.Raise
import Luna.Manager.System.Host
import Luna.Manager.System.Env
import Luna.Manager.Version
import Luna.Manager.Installer
import Luna.Manager.Repository
import Luna.Manager.Shell
import Luna.Manager.System.Path
import Luna.Manager.Network

-- FIXME: remove imports below before release:
import qualified Data.Yaml as Yaml


handleTopLvlError :: MonadIO m => SomeException -> m ()
handleTopLvlError e = do
    putStrLn $ "Fatal: " <> displayException e

main :: IO ()
main = do
    handleAll handleTopLvlError
        $ evalDefHostConfig @EnvConfig
        $ evalDefHostConfig @InstallConfig
        $ evalDefHostConfig @RepoConfig
        $ runInstaller def
        -- $ runInstaller (def & selectedComponent .~ Just "xstudio")

    putStrLn $ convert $ Yaml.encode $ hardcodedRepo
