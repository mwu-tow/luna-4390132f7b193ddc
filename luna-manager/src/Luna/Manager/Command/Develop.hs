module Luna.Manager.Command.Develop where

import Prologue
import Luna.Manager.Command.Options
import Luna.Manager.Network
import Luna.Manager.System.Env
import Control.Monad.Raise
import Control.Monad.State.Layered
import Luna.Manager.Shell.Shelly   (MonadSh)


import qualified Luna.Manager.Shell.Shelly as Shelly
import qualified Luna.Manager.Archive      as Archive


run :: (MonadIO m, MonadException SomeException m, MonadGetter EnvConfig m, MonadSh m) => DevelopOpts -> m ()
run opts = do
    Shelly.mkdir_p "luna"
    stackArchive <- downloadWithProgressBar "https://github.com/commercialhaskell/stack/releases/download/v1.5.1/stack-1.5.1-linux-x86_64-static.tar.gz"
    stackPath    <- Archive.unpack stackArchive
    print stackPath
    print "hello"
