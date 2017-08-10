module Luna.Manager.Command.Develop where

import Prologue hiding (FilePath)
import Luna.Manager.Command.Options
import Luna.Manager.Network
import Luna.Manager.System.Env
import Control.Monad.Raise
import Control.Monad.State.Layered
import Luna.Manager.Shell.Shelly   (MonadSh)
import Luna.Manager.System.Env     (move)
import Filesystem.Path.CurrentOS   (FilePath, (</>), encodeString, decodeString, toText, basename, hasExtension, parent)

import qualified Luna.Manager.Shell.Shelly as Shelly
import qualified Luna.Manager.Archive      as Archive


run :: (MonadIO m, MonadException SomeException m, MonadGetter EnvConfig m, MonadSh m) => DevelopOpts -> m ()
run opts = do
    let devPath   = "luna-workspace"
        toolsPath = devPath </> "tools"
        stackPath = toolsPath </> "stack"
    Shelly.mkdir_p toolsPath

    -- Stack installation
    stackArch  <- downloadWithProgressBar "https://github.com/commercialhaskell/stack/releases/download/v1.5.1/stack-1.5.1-linux-x86_64-static.tar.gz"
    stackArch' <- Archive.unpack stackArch
    move stackArch' stackPath

    -- cloning repo
    -- Shelly.sh


    print stackArch'
    print "hello"


-- - luna-workspace
--   - tools
--     - stack
--   - components
--     - luna
--     - luna-studio
--
