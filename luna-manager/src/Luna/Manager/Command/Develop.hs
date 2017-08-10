module Luna.Manager.Command.Develop where

import Prologue hiding (FilePath)
import Luna.Manager.Command.Options
import Luna.Manager.Network
import Luna.Manager.System.Env
import Control.Monad.Raise
import Control.Monad.State.Layered
import Luna.Manager.Shell.Shelly   (MonadSh, MonadShControl)
import Filesystem.Path.CurrentOS   (FilePath, (</>), encodeString, decodeString, toText, basename, hasExtension, parent)

import qualified Luna.Manager.Shell.Shelly as Shelly
import qualified Luna.Manager.Archive      as Archive

-- TODO: To refactor
instance Convertible FilePath Text where
    convert = convert . encodeString

run :: (MonadIO m, MonadException SomeException m, MonadGetter EnvConfig m, MonadSh m, MonadShControl m) => DevelopOpts -> m ()
run opts = do
    root <- Shelly.pwd
    let devPath   = root      </> "luna-workspace"
        toolsPath = devPath   </> "tools"
        appsPath  = devPath   </> "apps"
        stackPath = toolsPath </> "stack"
        stackBin  = stackPath </> "stack"
    Shelly.mkdir_p toolsPath
    Shelly.mkdir_p appsPath

    -- Stack installation
    let stackName    = "stack"
        stackVersion = "1.5.1"
    putStrLn . convert $ "Downloading " <> stackName <> " (" <> stackVersion <> ")"
    stackArch  <- downloadWithProgressBar $ "https://github.com/commercialhaskell/stack/releases/download/v" <> stackVersion <> "/stack-" <> stackVersion <> "-linux-x86_64-static.tar.gz"
    stackArch' <- Archive.unpack stackArch
    Shelly.mv stackArch' stackPath

    -- cloning repo
    let appName  = "luna-studio"
        repoPath = "git@github.com:luna/" <> convert appName <> ".git"
        appPath  = appsPath </> appName
    putStrLn . convert $ "Clonning repository " <> repoPath
    Shelly.run "git" ["clone", repoPath, convert appPath]

    -- building backend
    putStrLn "Building Luna Studio Backend"
    Shelly.chdir (appPath </> "build" </> "backend") $ do
        Shelly.run stackBin ["build", "--copy-bins", "--fast", "--install-ghc", convert appPath]

    return ()



-- - luna-workspace
--   - apps
--     - luna
--     - luna-studio
--   - tools
--     - stack
--
