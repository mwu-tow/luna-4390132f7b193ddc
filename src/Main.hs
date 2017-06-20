module Main where

import Prologue hiding (FilePath)

import Control.Monad.Raise
import Luna.Manager.System.Host
import Luna.Manager.System.Env
import Luna.Manager.Component.Version
import Luna.Manager.Command.Install
import Luna.Manager.Component.Repository
import Luna.Manager.System.Path
import Luna.Manager.Network
import Luna.Manager.Command.Options

-- FIXME: remove imports below before release:
import qualified Data.Yaml as Yaml

import Luna.Manager.Command.Options ()


handleTopLvlError :: MonadIO m => SomeException -> m ()
handleTopLvlError e = do
    putStrLn $ "Fatal: " <> displayException e

main :: IO ()
main = do
    handleAll handleTopLvlError
        $ evalOptionsParserT
        $ evalDefHostConfig @EnvConfig
        $ evalDefHostConfig @InstallConfig
        $ evalDefHostConfig @RepoConfig
        $ runInstaller def
        -- $ runInstaller (def & selectedComponent .~ Just "xstudio")

    putStrLn $ convert $ Yaml.encode $ hardcodedRepo
